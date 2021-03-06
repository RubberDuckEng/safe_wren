import re
from os import listdir
from pathlib import Path
from os.path import abspath, dirname, isdir, join, realpath


word_counts = {}


def collect_words(file_name):
    if not file_name.endswith(".wren.txt"):
        return
    with open(file_name) as results_file:
        for line in results_file:
            for word in line.split():
                count = word_counts.get(word, 0)
                word_counts[word] = count + 1


def collect_lines(file_name):
    if not file_name.endswith(".wren.txt"):
        return
    with open(file_name, encoding="utf-8") as results_file:
        for line in results_file:
            line = line.strip()
            # Remove error boilerplate
            # Explicitly don't remove "Expeted stack trace"
            if line == "" or line.startswith("Expected return code 0") or line.startswith('Unexpected') or line.startswith('Missing expected output') or line.startswith('Missing expected error'):
                continue
            # Remove path lines (from imports?)
            if (line.startswith("wren_c/") or line.startswith("test")) and line.endswith(".wren"):
                continue
            # Remove variants of line-numbers:
            line = re.sub(r'\[.*?\] ', '', line)
            line = re.sub(r'\d+\.\.\d+', '', line)
            line = re.sub(r'line: \d+', 'line: ', line)
            line = re.sub(r'line \d+', 'line ', line)
            count = word_counts.get(line, 0)
            word_counts[line] = count + 1


def walk(dir, callback, ignored=None):
    """
    Walks [dir], and executes [callback] on each file unless it is [ignored].
    """

    if not ignored:
        ignored = []
    ignored += [".", ".."]

    dir = abspath(dir)
    for file in [file for file in listdir(dir) if not file in ignored]:
        nfile = join(dir, file)
        if isdir(nfile):
            walk(nfile, callback)
        else:
            callback(nfile)


safe_wren_DIR = dirname(dirname(realpath(__file__)))
TEST_RESULTS_DIR = join(safe_wren_DIR, "test_results")
SCRIPT_PATH = Path(__file__).as_posix()

walk(join(safe_wren_DIR, 'test_results'), collect_lines)

items = list(
    sorted(word_counts.items(), key=lambda x: (-x[1], x[0])))

words = map(lambda item: f"{item[1]} : {item[0]}", items)


to_fix_path = join(safe_wren_DIR, 'common_test_errors.txt')

with open(to_fix_path, 'w', encoding="utf8") as to_fix_file:
    to_fix_file.write(f"Generated by {SCRIPT_PATH}\n")
    to_fix_file.write("Most common errors seen in test_results/**:\n")
    to_fix_file.write("occurances : error text\n")
    to_fix_file.write('\n'.join(words))
