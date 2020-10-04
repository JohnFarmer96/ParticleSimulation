#!/bin/sh

# Delete old data
rm data/*.dat

# Run main program
make run

# Execute visualization script
/usr/bin/python3 python/main.py