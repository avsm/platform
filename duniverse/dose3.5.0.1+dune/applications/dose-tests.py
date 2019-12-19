#!/usr/bin/python

import unittest
from subprocess import Popen, PIPE
import difflib
import uuid
import os,sys,time,glob
import argparse
from itertools import groupby, ifilter
import yaml
import copy

try :
    from yaml import CBaseLoader as yamlLoader
except ImportError:
    from yaml import BaseLoader as yamlLoader
    warning('YAML C-library not available, falling back to python')

import filecmp
import cStringIO
from sets import Set

def which(program):
    path = "/usr/share/cudf/solvers/"
    exe_file = os.path.join(path, program)
    return os.path.isfile(exe_file)

def which(program):
    import os
    def is_exe(fpath):
        return os.path.isfile(fpath) and os.access(fpath, os.X_OK)

    fpath, fname = os.path.split(program)
    if fpath:
        if is_exe(program):
            return program
    else:
        for path in os.environ["PATH"].split(os.pathsep):
            path = path.strip('"')
            exe_file = os.path.join(path, program)
            if is_exe(exe_file):
                return exe_file

    return None

def removeTmpFiles() :
    for f in glob.glob("/tmp/apt-cudf-universe*.cudf") :
        os.remove(f)

class Ignore(Exception):
    pass

def convert(d) :
    def aux(e) :
        if isinstance(e, dict) :
            return frozenset(sorted([(k,aux(v)) for (k,v) in e.items()]))
        elif isinstance(e, list) :
            return frozenset(sorted([aux(v) for v in e]))
        else :
            return e
    return sorted([aux(e) for e in d])

def parse822(f,filter) :
    records = []
    for empty, record in groupby(ifilter(lambda s: not s.startswith('#'),open(f)), key=str.isspace):
        if not empty:
            l = map(lambda s : tuple(s.split(': ')), record)
            l = map(lambda (k,v) : (k,v.rstrip()), l)
            if (len(l) > 0):
                try :
                    pairs = ((k, filter((k,v.strip()))) for k,v in l)
                    records.append((pairs))
                except Ignore :
                    continue

    l = sorted([sorted(e) for e in records])
    return [frozenset(e) for e in l]

def parseedsp(f):
    fields = ['Package','Architecture']

    def filter(k,s,fields) :
        if k in fields :
            return s

    return parse822(f,filter)

def parsedistcheck(f) :
    cnf_fields = ['conflict','depends','provides','recommends']

    def cnf(k,s) :
        if k == "preamble" : raise Ignore
        if k in cnf_fields :
            l = s.split(',')
            ll = map(lambda s : s.split('|'), l)
            return ll
        else :
            return s

    return parse822(f,filter)

def parseyaml(f) :
    print "yaml %s" % f
    l = []
    if os.path.getsize(f) > 0 :
        data = yaml.load(open(f), Loader=yamlLoader)
        report = data.get('report', [])
        l = convert(report)
    return l

def parsetext(f) :
    l = sorted(open(f).readlines())
    return [frozenset(sorted(e)) for e in l]

def diff_aux(expectedfile,resultfile,parser):
    if filecmp.cmp(expectedfile,resultfile) :
        return True
    else :
        expected = parser(expectedfile)
        result = parser(resultfile)
        matcher = difflib.SequenceMatcher(None, expected, result)
        if matcher.ratio() == 1.0 :
            print "Warning ! Expected result and actual result are not identical."
            print "The order is not the same."
            return True
        else :
            if False :
                diff = difflib.unified_diff(open(expectedfile).readlines(),open(resultfile).readlines())
                sys.stdout.writelines(list(diff))
            return False

def diff_yaml(expectedfile,resultfile):
    return diff_aux(expectedfile,resultfile,parseyaml)

def diff_822(expectedfile,resultfile):
    return diff_aux(expectedfile,resultfile,parsedistcheck)

def diff_text(expectedfile,resultfile):
    return diff_aux(expectedfile,resultfile,parsetext)

def diff_edsp(expectedfile,resultfile):
    return diff_aux(expectedfile,resultfile,parseedsp)
    
def test_application(self,expected_file,cmd,diff,exitcode):
    uid = uuid.uuid1()
    mytmp = False
    if not os.path.exists("tmp"):
        mytmp = True
        os.makedirs("tmp")

    output_file = "tmp/%s.cudf" % uid
    output = open(output_file,'w')
    p = Popen(cmd, stdout=output)
    p.communicate()
    rc = p.returncode if exitcode is not None else None
    if rc == exitcode :
        ec = True
    else :
        print "ExitCode = %d" % rc
        ec = False
    d = diff(expected_file,output_file)
    output.close()
    os.remove(output_file)
    if mytmp :
        os.rmdir("tmp")
    self.assertTrue(d)
    self.assertTrue(ec)

class DoseTests(unittest.TestCase):
    def __init__(self, test):
        super(DoseTests, self).__init__()
        self.name = test['Name'] 
        self.group = test['Group']
        self.comment = test['Comment'] if 'Comment' in test else None
        self.expected = test['Expected'] 
        self.cmd = test['Cmd'].split(' ') + test['Input'].split(' ')
        self.exitcode = int(test['ExitCode']) if 'ExitCode' in test else None
        self.enctype = test['Type'] if 'Type' in test else '822'
        self.solver = test['Solver'] if 'Solver' in test else None
        if self.enctype == '822' :
            self.difftype = diff_822
        elif self.enctype  == 'yaml' :
            self.difftype = diff_yaml
        elif self.enctype  == 'text' :
            self.difftype = diff_text
        elif self.enctype  == 'edsp' :
            self.difftype = diff_text
        else :
            self.difftype = diff_text
    def shortDescription(self):
        if self.comment :
            return "Description : " + self.comment + "\n" + "\nExpected file : %s" % self.expected + "\n"
        else :
            s =     "Test : %s" % self.name
            s = s + "\nGroup : %s" % self.group
            s = s + "\n" + "Cmd : " + " ".join(self.cmd)
            if self.solver : 
                s = s + "\n" + "Solver : " + self.solver
            s = s + "\nExpected file : %s" % self.expected
            s = s + "\nExpected exitcode : %d" % self.exitcode if self.exitcode is not None else s
            return s + "\n"
    def runTest(self):
        test_application(self,self.expected,self.cmd,self.difftype,self.exitcode)
    def tearDown(self):
        removeTmpFiles()

def suite(f,runtest,rungroup,slow=False):
    suite = unittest.TestSuite()
    groups = Set()
    tests = Set()
    groupFound=False
    testFound=False
    def addtest(s) :
        # we run the slow tests only if 
        if (slow and 'Speed' in s and s['Speed'] == 'slow') :
            return
        else :
            #default we run the test
            if 'edsp' in s['Type'] :
                solvers = ['aspcud','packup','mccs-lpsolve']
                if 'Solver' in s :
                    solvers = [x.strip() for x in s['Solver'].split(',')]
                for solver in solvers :
                    if which(solver) :
                        ss = copy.deepcopy(s)
                        f = "%s-%s" % (ss['Expected'],solver)
                        ss['Expected'] = f if os.path.isfile(f) else ss['Expected']
                        ss['Solver'] = solver
                        ss['Cmd'] = ss['Cmd'] + " --solver " + solver
                        suite.addTest(DoseTests(ss))
            else :
                suite.addTest(DoseTests(s))
    for stanza in parse822(f,lambda s: s[1]):
        s = dict(stanza)
        if s['Name'] not in runtest and 'Ignore' in s and s['Ignore'] == 'yes' :
            continue
        groups.add(s['Group'])
        if (len(runtest) == 0 and len(rungroup) == 0) :
            addtest(s)
        elif s['Name'] in runtest :
            testFound=True
            addtest(s)
        elif len(rungroup) > 0 and s['Group'] in rungroup :
            groupFound=True
            addtest(s)
    if len(runtest) != 0 and testFound == False :
        print "Test(s) [%s] Not found" % (','.join(str(p) for p in runtest)) 
        print "Tests available [%s]" % (','.join(str(p) for p in tests))
    if len(rungroup) != 0 and groupFound == False :
        print "Group(s) [%s] Not found" % (','.join(str(p) for p in rungroup))
        print "Groups available [%s]" % (','.join(str(p) for p in groups))

    return suite

def fixtest(expected_file,cmd):
    output = open(expected_file,'w')
    p = Popen(cmd, stdout=output)
    p.communicate()
    output.close()

def main():
    parser = argparse.ArgumentParser(description='Unit test for Dose applications')
    parser.add_argument('-d', '--diff', action='store_true', default=False, help="Print diffs")
    parser.add_argument('-s', '--slow', action='store_true', default=False, help="Run slow tests") 
    parser.add_argument('--runtest', nargs=1, default=[]) 
    parser.add_argument('--rungroup', nargs=1, default=[]) 
    parser.add_argument('--fixtest', nargs=1, default=[]) 
    parser.add_argument('inputfile', type=str, nargs=1, help="test file")
    args = parser.parse_args()

    if len(args.fixtest) > 0 :
        f = args.inputfile[0]
        for stanza in parse822(f,lambda s: s[1]):
            s = dict(stanza)
            if args.fixtest[0] == s['Name'] :
                cmd = s['Cmd'].split(' ') + s['Input'].split(' ')
                expected = s['Expected']
                print "Overwriting expected file: %s" % expected
                fixtest(expected,cmd)
    else :
        unittest.TextTestRunner(verbosity=2).run(suite(args.inputfile[0],args.runtest,args.rungroup,args.slow))

if __name__ == '__main__':
    main()

