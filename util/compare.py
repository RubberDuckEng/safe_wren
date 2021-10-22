#!/usr/bin/env python

from __future__ import print_function

from argparse import ArgumentParser
from collections import defaultdict
from os import listdir, makedirs, remove, environ, getcwd
from os.path import abspath, basename, dirname, isdir, isfile, join, realpath, relpath, splitext
import re
from subprocess import Popen, PIPE
import sys
from threading import Timer
import platform

parser = ArgumentParser()
parser.add_argument('test_path')
args = parser.parse_args(sys.argv[1:])

safe_wren_DIR = dirname(dirname(realpath(__file__)))
WREN_DIR = join(safe_wren_DIR, 'wren_c')
safe_wren_TEST = join(safe_wren_DIR, 'target', 'debug',
                      'wren_test')
WREN_C_TEST = join(WREN_DIR, 'bin', 'wren_test')


def run(runner, test_path):
    proc = Popen([runner, test_path],
                 stdin=PIPE, stdout=PIPE, stderr=PIPE)

    out, err = proc.communicate(None)
    out = out.decode("utf-8", "replace").replace('\r\n', '\n')
    err = err.decode("utf-8", "replace").replace('\r\n', '\n')
    return out + err


print("wren_c")
print(run(WREN_C_TEST, args.test_path))

print("safe_wren")
print(run(safe_wren_TEST, args.test_path))
