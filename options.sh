#!/bin/sh
cd src/sql
aquarius --clear-cache
aquarius -f -i ../../config/kit-sql.options -a generate
