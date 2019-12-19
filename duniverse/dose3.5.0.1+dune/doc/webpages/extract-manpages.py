#!/usr/bin/python

import os,sys,time
import argparse

def extract(html_doc):
    from bs4 import BeautifulSoup
    soup = BeautifulSoup(html_doc)
    body = soup.body
    body.find('ul', id='index').extract()
    name = soup('h1',id='NAME',limit=1)[0].find_next_sibling()
    description = soup('h1',id='DESCRIPTION',limit=1)[0].find_next_sibling()
    soup('h1',id='NAME',limit=1)[0].find_next_sibling().extract()
    body.find('h1', id='NAME').extract()

    for a in soup.findAll('h1'):
        a.string = a.string.lower().capitalize()
        a.name = 'h3'

    print name.string.lower().capitalize() 
    return (body,name.string.lower().capitalize(),description)

def main():
    parser = argparse.ArgumentParser(description='Ocamldoc body extractor')
    parser.add_argument('inputfile', type=str, nargs='*', help="test file")
    args = parser.parse_args()
 
    f_man = open('src/man.mdwn','w')
    print >> f_man, '[[!meta title="Dose Based Applications"]]\n'
    for f in args.inputfile : 
        (b,n,d) = extract(open(f))
        fname = os.path.basename(f)
        print >> f_man, "### [[%s]]" % (os.path.splitext(fname)[0])
        print >> f_man, "%s\n" % d
        f_doc = open(os.path.join('src/man',os.path.basename(f)),'w')
        print >> f_doc, '[[!meta title="%s"]]\n' % n
        print >> f_doc, b

if __name__ == '__main__':
    main()
