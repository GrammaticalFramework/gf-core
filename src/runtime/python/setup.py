from distutils.core import setup, Extension
import os

includes = os.getenv('EXTRA_INCLUDE_DIRS','').split(':')
if includes==['']:
    includes=[]
libraries = os.getenv('EXTRA_LIB_DIRS','').split(':')
if libraries==['']:
    libraries=[]

pgf_module = Extension('pgf',
                       sources = ['pypgf.c'],
                       extra_compile_args = ['-std=c99'],
                       include_dirs = includes,
                       library_dirs = libraries,
                       libraries = ['gu', 'pgf'])

setup (name = 'pgf',
       version = '1.0',
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
