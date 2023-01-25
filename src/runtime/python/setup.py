from setuptools import setup, Extension
import os
import platform

on_windows = platform.system() == 'Windows'

includes = os.getenv('EXTRA_INCLUDE_DIRS','').split(':')
if includes==['']:
    includes=[]
libraries = os.getenv('EXTRA_LIB_DIRS','').split(':')
if libraries==['']:
    libraries=[]

if on_windows:
    extra_sources = [f for f in os.listdir('../c/pgf') if f.endswith('.cxx')]
else:
    extra_sources = []

pgf_module = Extension(
    'pgf',
    sources = [
        'pypgf.c',
        'expr.c',
        'ffi.c',
        'transactions.c'
    ]+extra_sources,
    extra_compile_args = [] if on_windows else ['-std=c99', '-Werror', '-Wno-error=unused-variable', '-Wno-comment'],
    include_dirs = includes,
    library_dirs = libraries,
    libraries = [] if on_windows else ['pgf'])

setup(
    name = 'pgf',
    version = '2.0',
    description = 'Python bindings to the Grammatical Framework\'s PGF runtime',
    long_description="""\
Grammatical Framework (GF) is a programming language for multilingual grammar applications.
This package provides Python bindings to GF runtime, which allows you to \
parse and generate text using GF grammars compiled into the PGF format.
""",
    url='https://www.grammaticalframework.org/',
    author='Krasimir Angelov',
    author_email='kr.angelov@gmail.com',
    license='BSD',
    ext_modules = [pgf_module])
