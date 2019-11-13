#!/bin/bash

runhaskell Test.hs && runhaskell Compiler.hs > a.py && python3 a.py && rm a.py
