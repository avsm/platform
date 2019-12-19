#!/usr/bin/python

import argparse

import yaml

def main():
    parser = argparse.ArgumentParser(description='')
    parser.add_argument('inputfile', type=str, nargs=1, help="distcheck report")
    args = parser.parse_args()
    doc = yaml.load(file(args.inputfile[0],'r'))
    print doc['report'][0]

if __name__ == '__main__':
    main()
