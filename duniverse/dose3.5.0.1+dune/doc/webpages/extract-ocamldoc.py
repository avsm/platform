#!/usr/bin/python

import os,sys,time
import argparse

def extract(html_doc):
    from bs4 import BeautifulSoup
    soup = BeautifulSoup(html_doc)

    for a in soup.findAll('a'):
        if 'href' in a :
            l = a['href'][:-5]
            a['href'] = "../" + l + "/" + "index.html"
     
    print soup.body

def main():
    parser = argparse.ArgumentParser(description='Ocamldoc body extractor')
    parser.add_argument('inputfile', type=str, nargs=1, help="test file")
    args = parser.parse_args()
 
    f = args.inputfile[0]
    extract(open(f))

if __name__ == '__main__':
    main()
