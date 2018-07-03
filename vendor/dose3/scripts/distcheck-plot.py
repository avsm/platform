#!/usr/bin/python
import argparse
import csv
import datetime as dt
import matplotlib.pyplot as plt
from matplotlib import dates
import os.path
from itertools import groupby
from operator import itemgetter, attrgetter, methodcaller
import collections

# takes as a input a cvs file and a list of distributions and
# creates a plot of the evolution of broken and outdated packages
# vs time.

# cvs format "date suite packages broken (outdated)"
# ex : 20111125 unstable 35056 337 (73)

# parse a time line file of the form :
# Title: ...
# Date: Mar 25 2013
# <other infos>: ignored
def parse_timeline(fname,timeline) :
    records = []
    for empty, record in groupby(open(fname), key=str.isspace):
      if not empty:
        pairs = map(lambda s : s.split(': '), record)
        pairs = dict(map(lambda (k,v) : (k,v.rstrip()), pairs))
        pairs['Date'] = dt.datetime.strptime(pairs['Date'],"%b %d %Y")
        pairs['Timeline'] = timeline
        records.append(pairs)
    return records

def aggregate_timeline(timelines) :
    t = sorted(timelines,key=lambda t : t['Date'])
    l = []
    i=0
    while i < len(t) - 1 :
        if t[i]['Timeline'] == 'release' :
            if t[i+1]['Timeline'] == 'release' :
                #l.append(t[i])
                i=i+1
            else :
                i=i+1
        elif t[i]['Timeline'] == 'freeze' :
            if t[i+1]['Timeline'] == 'release' :
                l.append((t[i],t[i+1]))
                i=i+2
            else :
                i=i+1
        else :
            i=i+1
    return l

def initdict(dists,distname) :
    dists[distname] = {'date' : [], 'total' : [], 'broken' : [], 'outdated' : [] }
    return dists

def parse(dataset,outdated=False):
    dists = collections.OrderedDict()
    csv_reader = csv.reader(dataset,delimiter=' ')
    for line in sorted(csv_reader,key=lambda k: k[0]):
        distname = line[1]
        if distname not in dists :
            dists = initdict(dists,distname)

        dists[distname]['date'].append(dt.datetime.strptime(line[0],'%Y%m%d'))
        dists[distname]['total'].append(int(line[2]))
        dists[distname]['broken'].append(int(line[3]))
        if outdated : 
            dists[distname]['outdated'].append(int(line[4]))

    return dists

def plot(dists,output,title,intervals,outdated=False) :

    distlist = dists.keys()
    fig = plt.figure()
    fig.suptitle(title)
    plotsidx = [311,312,313] if outdated else [211,212]
    ax1 = fig.add_subplot(plotsidx[0],title='Total Packages vs Time')
    ax2 = fig.add_subplot(plotsidx[1],title='Non-Installable Packages vs Time')
    if outdated : 
        ax3 = fig.add_subplot(plotsidx[2],title='Outdated Packages vs Time')
    for k in distlist :
        ax1.plot(dists[k]['date'],dists[k]['total'],',-',label=k.capitalize())
        ax2.plot(dists[k]['date'],dists[k]['broken'],',-',label=k.capitalize())
        if intervals :
            mdate = min(dists[k]['date'])
            for (f,r) in intervals :
                if f['Date'] > mdate :
                    ax2.axvspan(f['Date'],r['Date'], alpha=0.5, color='red')
        if outdated :
            ax3.plot(dists[k]['date'],dists[k]['outdated'],',-',label=k.capitalize())


    if len(distlist) > 1 :
        ax1.legend(loc='upper left')
        ax2.legend(loc='upper left')
        #ax2.legend()
        if outdated :
            ax3.legend(loc='upper left')

    fig.autofmt_xdate()

    plt.savefig(output)

def multiplot(dists,output,title,timelines,intervals) :

    fig = plt.figure()
    fig.suptitle(title)
    subplotnum = len(dists)
    #fig.set_figheight((subplotnum+1) * 3)
    fig.set_figheight(24)

    ax = {}
    plotsidx = []
    for i in range(1, subplotnum+2) :
        plotsidx.append(int(("%d1%d") % (subplotnum+1,i)))

    print plotsidx

    # First graph puts all dists together in one
    ax[0] = fig.add_subplot(plotsidx[0],title='Total Packages vs Time')
    for k in dists :
        ax[0].plot(dists[k]['date'],dists[k]['total'],',-',label=k.capitalize())
    ax[0].legend(loc='upper left')

    i = 1
    for n in dists.keys() :
        ax[i] = fig.add_subplot(plotsidx[i])
        if i == 1 :
            ax[i].set_title('Non-Installable Packages vs Time')
        ax[i].plot(dists[n]['date'],dists[n]['broken'],',-',label=n.capitalize())
        ax[i].legend(loc='upper left')
        if intervals :
            mdate = min(dists[k]['date'])
            for (f,r) in intervals :
                if f['Date'] > mdate :
                    ax[i].axvspan(f['Date'],r['Date'], alpha=0.5, color='red')
        i += 1

    fig.autofmt_xdate()
    plt.savefig(output)

# plot two distribution with different scales
def multiscale(dists,dist1,dist2,output,title,outdated=False) :

    fig = plt.figure()
    fig.suptitle(title)
    fig.autofmt_xdate()

    plotsidx = [311,312,313] if outdated else [211,212]
    ax1 = fig.add_subplot(plotsidx[0],title='Total Packages vs Time')
    ax1.plot(dists[dist1]['date'],dists[dist1]['total'],',-',label=dist1.capitalize())
    ax1.plot(dists[dist2]['date'],dists[dist2]['total'],',-',label=dist2.capitalize())
    ax1.legend(loc='upper left')
    ax1.xaxis.set_visible(False)

    ax2 = fig.add_subplot(plotsidx[1],title='Non-Installable Packages vs Time')
    ax2.plot(dists[dist1]['date'],dists[dist1]['broken'],'o-',label=dist1.capitalize())
    ax2.xaxis.set_visible(False)

    ax22 = ax2.twinx() 
    ax22.plot(dists[dist2]['date'],dists[dist2]['broken'],'s-',label=dist2.capitalize())
    ax22.set_ylim(0, 70)

    if outdated : 
        ax3 = fig.add_subplot(plotsidx[2],title='Outdated Packages vs Time')
        ax3.plot(dists[dist1]['date'],dists[dist1]['outdated'],'o-',label=dist1.capitalize())

        ax33 = ax3.twinx() 
        ax33.plot(dists[dist2]['date'],dists[dist2]['outdated'],'gs-',label=dist2.capitalize())
        ax33.set_ylim(0, 10)
        # ax33.set_yticks([-1,0,1])

        plt.setp(ax3.xaxis.get_majorticklabels(), rotation=30)

    plt.savefig(output)

def main():
    parser = argparse.ArgumentParser(description='plot outdated/broken')
    parser.add_argument('-v', '--verbose')
    parser.add_argument('-d', '--debug', action='store_true', default=False)
    parser.add_argument('-o', '--output', action='store')
    parser.add_argument('-t', '--title', action='store')
    parser.add_argument('-s', '--split', action='store_true', default=False)
    parser.add_argument('--ms', action='store_true', default=False, help="multi plot")
    parser.add_argument('--mp', action='store_true', default=False, help="multi scale")
    parser.add_argument('--releases', type=str, nargs=1, help="release timeline")
    parser.add_argument('--freezes', type=str, nargs=1, help="freeze timeline")
    parser.add_argument('--outdated', action='store_true', default=False)
    parser.add_argument('dataset', type=str, nargs=1, help="dataset")
    args = parser.parse_args()
 
    fname = args.dataset[0]

    dataset = open(fname)
    dists = parse(dataset)
    dataset.close()

    title = args.title[0] if args.title else ""

    timelines = {}
    intervals = []
    if args.releases :
        timelines['releases'] = parse_timeline(args.releases[0],'release')
    if args.freezes :
        timelines['freezes'] = parse_timeline(args.freezes[0],'freeze')
    if args.releases and args.freezes :
        intervals = aggregate_timeline(timelines['releases'] + timelines['freezes'])

    if args.ms :
        output = "aggregate-%s-ms.png" % os.path.splitext(os.path.basename(fname))[0]
        multiscale(dists, output, title, args.outdated)
    elif args.mp :
        output = "aggregate-%s-mp.png" % os.path.splitext(os.path.basename(fname))[0]
        multiplot(dists, output, title, timelines, intervals)
    else :
        if args.split :
            # different graphs, one for each suite
            for d in dists.keys() :
                output = "%s-%s.png" % (d,os.path.splitext(os.path.basename(fname))[0])
                plot(dists, [d] , output, title, intervals, args.outdated)
        else :
            # all in one graph
            output = "aggregate-%s.png" % os.path.splitext(os.path.basename(fname))[0]
            plot(dists, output, title, intervals, args.outdated)

if __name__ == '__main__':
    main()

