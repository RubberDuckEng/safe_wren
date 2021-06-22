import re
from os import listdir
from os.path import abspath, dirname, isdir, join, realpath


word_counts = {}


def collect_words(file_name):
    with open(file_name) as results_file:
        for line in results_file:
            for word in line.split():
                count = word_counts.get(word, 0)
                word_counts[word] = count + 1


def collect_lines(file_name):
    with open(file_name, encoding="utf-8") as results_file:
        for line in results_file:
            line = line.strip()
            # Remove error boilerplate
            if line == "" or line.startswith('Expected') or line.startswith('Unexpected') or line.startswith('Missing expected'):
                continue
            # Remove path lines (from imports?)
            if (line.startswith("wren_c/") or line.startswith("test")) and line.endswith(".wren"):
                continue
            # Remove variants of line-numbers:
            line = re.sub(r'\[.*?\] ', '', line)
            line = re.sub(r'\d+\.\.\d+', '', line)
            line = re.sub(r'line: \d+', '', line)
            line = re.sub(r'line \d+', '', line)
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


WREN_RUST_DIR = dirname(dirname(realpath(__file__)))
TEST_RESULTS_DIR = join(WREN_RUST_DIR, "test_results")

walk(join(WREN_RUST_DIR, 'test_results'), collect_lines)

items = list(reversed(sorted(word_counts.items(), key=lambda item: item[1])))

words = map(lambda item: f"{item[1]} : {item[0]}", items)


to_fix_path = join(WREN_RUST_DIR, 'common_test_errors.txt')

with open(to_fix_path, 'w') as to_fix_file:
    to_fix_file.write("Most common errors seen in test_results/**:")
    to_fix_file.write("occurances : error text")
    to_fix_file.write('\n'.join(words))
