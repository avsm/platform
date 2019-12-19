#!/usr/bin/python

import argparse
import os
import sys
import yaml
import glob


# create csv file containing aggregate information
# format : date suite total broken outdated

# input : distcheck / oudated yaml output and timing
# information
# ex : $file.distcheck / $file.distcheck.time /
#      $file.outdated / $file.outdated.time

def crunch(dist,time,distcheck,outdated=None) :

    data = yaml.load(distcheck)
    broken_debcheck = data['broken-packages']
    total_packages = data['total-packages']

    if outdated :
        data = yaml.load(outdated)
        broken_outdated = data['broken-packages']

    if outdated :
        print "%s %s %s %s %s" %(time,dist,total_packages,broken_debcheck,broken_outdated)
    else :
        print "%s %s %s %s" %(time,dist,total_packages,broken_debcheck)

def main():
    parser = argparse.ArgumentParser(description='distcheck/outdated data cruncing')
    parser.add_argument('-v', '--verbose')
    parser.add_argument('-d', '--debug', action='store_true', default=False)
    parser.add_argument('--outdated', action='store_true', default=False)
    parser.add_argument('archive', type=str, nargs=1, help="archive")
    parser.add_argument('dists', type=str, nargs=1, help="dists")
    args = parser.parse_args()

    dists = args.dists[0]
    archive = args.archive[0]

    for fname in glob.iglob(archive+"/*.distcheck") :
        time = os.path.splitext(os.path.basename(fname))[0]
        if os.path.getsize(fname) > 0 :
            distcheck = open(fname)
            crunch(dists,time,distcheck)
            distcheck.close()

if __name__ == '__main__':
    main()

