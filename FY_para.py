#!/usr/bin/env python

from ascar_parallel import StartCluster
import numpy as np
import os
import shutil

#def runFY(ndir, dirname, exe_path):
#def runFY(ndir, path, exe_path):
def runFY(ndir, root, path, exe_path):
    import os
    import shutil
    from subprocess import Popen
 
# Create folders for unfinished runs
    dirname = os.path.join(path, 'dir_%s' % ndir)
    if os.path.isdir(dirname):
        shutil.rmtree(dirname)
    print 'path= '+path
    print 'dirname= '+dirname
# Create sim.in.txt to store run number (for instance, 26th run)
    os.makedirs(dirname)
    filename = os.path.join(dirname, 'sim.in.txt')
    with open(filename, 'w') as f:
        f.write('%s' % ndir)

# Put all needed files into the folders for all unfinished runs:
# sim, table, input.dat, runwam.bat, and FY_Unix.py (through exe_path)
    pwd = os.getcwd()
    print 'in function runFY: pwd = '+pwd
    shutil.copy(exe_path, dirname)
    shutil.copy(root+'/sim', dirname)
    shutil.copy(root+'/table', dirname)
    shutil.copy(root+'/input.dat', dirname)
    shutil.copy(root+'/runwam.bat', dirname)
    os.chdir(dirname)

# Run FY_Unix in each created folder
#    p = Popen(exe_path).wait()
    p = Popen('./FY_Unix.py').wait()
    os.chdir(pwd)

    return



if __name__ == '__main__':
    ncpu_defult = 50
    nfiles = 50

# Find unfinished run # for simulation
    filename = os.path.join(os.getcwd(), 'finished.txt')	#if finished.txt does not exist, create one first.
    x=[]
    finished = []
    un_finished=[]
    with open(filename) as f:
        for line in f:
            x=line.strip().split(',')[0]
            x=int(x)
            finished.append(x)
    finished.sort()

    for n in range(nfiles):
        if n in finished:
            print str(n)+'  is finished. skip'
        else:
            un_finished.append(n)

# Creat Result folder
    root = os.getcwd()
    abs_path = os.path.join(os.getcwd(),'Result')

    #remove everything in path directory
# - pact
    #if os.path.isdir(abs_path):
    #    shutil.rmtree(abs_path)
    #os.makedirs(abs_path)
# - pact end
    if os.path.isdir(abs_path)<0:
        os.makedirs(abs_path)
    exe_path = os.path.join(os.getcwd(), 'FY_Unix.py')

# Create arrays for unfinished runs
    if finished==[]:
        runs = range(0, nfiles)
    else:
        runs = un_finished
    print runs
    if runs!=[]:
        ncpu=min(len(runs),ncpu_defult)
        print 'CPUs required: '+str(ncpu)
        parallel_roots = []
        parallel_paths = []
        exe_paths = []

        for run in runs:
            parallel_roots.append(root)
            parallel_paths.append(abs_path)
            exe_paths.append(exe_path)
    
        print 'Starting Parallel Run -----------------------------'
        print root
        with StartCluster(ncpu) as lview:
            lview.map(runFY, runs, parallel_roots, parallel_paths, exe_paths)
    else:
        print 'All runs are completed'

