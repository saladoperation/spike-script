#!/usr/bin/env run-script
module Application.Script.HelloWorld where

import Application.Script.Prelude

run :: Script
run = do
    putStrLn "Hello World!"
