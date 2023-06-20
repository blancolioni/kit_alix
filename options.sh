#!/bin/sh
cd src
aquarius -f -i ../config/kit.options -a generate
cd ..
cd src/sql
aquarius -f -i ../../config/kit-sql.options -a generate
cd ../..
