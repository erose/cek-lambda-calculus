#!/bin/bash

runhaskell Test.hs && runhaskell Compiler.hs > a.py && python a.py && rm a.py
