#!/usr/bin/env python

# ####################################
# ########  Define Functions  ########
# ####################################

# This program calculates the firm diversion of a new, inserted WR
# at a reservoir while all other existing water rights of the reservoir
# are simulated at their firm yield diversions. This is different 
# than in ExtraFY.py where Extra yield is calculated when all other
# existing wrs of the simulated reservoir are at their permitted diversions.

def ini_files_1(path1, path2, wamfile): 
    # (1) Create midinput.dis file once for all hydrology;
    # (2) Create 1 batch file and 2 misc filess  
      
    for fname in os.listdir(path2):
        if fname.startswith(wamfile+'.dis'): shutil.copy(path2+wamfile+'.dis',path1+'midinput.dis')

    # Create simu.txt, table.txt and runsim.bat files #########
    f1=open(path1+'simu.txt','w')
    f1.write('midinput \n')
    f1.write('midinput \n')
    f1.close() 

    f1=open(path1+'table.txt','w')
    f1.write('tab \n')
    f1.write('tab \n')
    f1.write('midinput \n')
    f1.close()

    f1=open(path1+'runwam.bat','w')
    f1.write('rm midinput.OUT \n')
    f1.write('rm midinput.MSS \n')
    f1.write('rm tab.TOU \n')
    f1.write('rm tab.TMS \n')
    f1.write('sim<simu.txt \n')
    f1.write('table<table.txt \n')
    f1.close()    
    return

def ini_storage(path2, wamfile, name, percent):     # wamfile+'.dat', wr to be altered
    # change initial reservoir stoarge
    res_name=['','','','','','','','','','','','']
    space='                                                 '
    file_ori=wamfile+'_ori.dat'
    file_new=wamfile+'.dat'
    for fname in os.listdir(path2):
        if fname.startswith(file_new): os.remove(path2+file_new)
    
    leng=len(name)  # length of the res_name
    num=leng/6      # number of reservoirs simulated    
    print 'number of reservoir is '+str(num)
    i=0
    while i<num:
        res_name[i]=name[i*6:(i+1)*6]
        i+=1
    
    f1=open(path2+file_ori,'r')
    lines=f1.readlines()
    f1.close()
    f2=open(path2+file_new,'w')
    
    for line in lines:
        y0=line[:2]
        k=0
        flag=0
        while k<num and flag==0:            
            if y0=='WS' and res_name[k] in line :
                if len(line.strip())<49: line=line.strip()+space
                x1=float(line[8:16])
                x2=line[40:48].strip()
                if x2=='': 
                    x2=0.0
                x3=float(x2)    # dead pool top
                dx=0.01*percent*(x1-x3)
                if dx<=0.0: dx=1.0                
                storage=int(x3+dx)
                y2='{:>8}'.format(str(storage))
                y1=line[:48]
                y3=line[56:]
                line=(y1+y2+y3).strip()
                line=line+'\n'
                flag=1
            k+=1
        f2.write(line)
    
    f2.close()
    return

def ini_files_2(path1, path2, wamfile, basinwidefy): 
    # (1) create midinput.eva, midinput.inf and midinput.fad for every new hydrology;

    # If you want basin-wide firm yield, do midinput.dat file here;
    # Otherwise, do it in ini_files_2
    # midinput.dat block ################################################
    if basinwidefy=='yes':
        for fname in os.listdir(path1):
            if fname.startswith('midinput.dat'): os.remove(path1+fname)
        shutil.copy(path2+wamfile+'.dat',path1+'midinput.dat')
    #-----------------------------------------------------------------------
        
    for fname in os.listdir(path1):
        if fname.startswith('midinput.eva'): os.remove(path1+fname)
        if fname.startswith('midinput.inf'): os.remove(path1+fname)
        if fname.startswith('midinput.FLO'): os.remove(path1+fname)
        if fname.startswith('midinput.fad'): os.remove(path1+fname)
    for fname in os.listdir(path2):
        if fname.startswith(wamfile+'.eva'): shutil.copy(path2+wamfile+'.eva',path1+'midinput.eva')
        if fname.startswith(wamfile+'.inf'): shutil.copy(path2+wamfile+'.inf',path1+'midinput.inf')
        if fname.startswith(wamfile+'.flo'): shutil.copy(path2+wamfile+'.flo',path1+'midinput.FLO')
        if fname.startswith(wamfile+'.fad'): shutil.copy(path2+wamfile+'.fad',path1+'midinput.fad')


def ini_files_3(path1, path2, wamfile, basinwidefy): 
    # (1) create midinput.dat for every new hydrology and every iteration;

    # If you do not want basin-wide firm yield (instead, you want individual firm yield), do midinput.dat file here;
    # Otherwise, do it in ini_files_1

    if basinwidefy=='no':
        for fname in os.listdir(path1):
            if fname.startswith('midinput.dat'): os.remove(path1+fname)
        shutil.copy(path2+wamfile+'.dat',path1+'midinput.dat')


def midinput_add(path1, name):    # Add an ADDINS water right for each of reservoirs / system
    f2=open(path1+'midinput1.dat','w')
    f1=open(path1+'midinput.dat','r')
    lines=f1.readlines()
    f1.close()
    
    name=name.strip()
    done=0
    for line in lines:
        f2.write(line)
        if done==0:            
            x=line[:2]
            y=line[2:8]
            if x=='WR':
                wrid=y

            z=name.find(y)
            if x=='WS'and z!=-1:      # found the reservoir /system
                print 'res: '+y
                addwr='WR'+wrid+'100000.0'+'        '+'30000101'+'                                '+'EXTRA_YLD_'+y
                f2.write(addwr+'\n'+line)    # add ADDINS wr
                done=1  # Done! A new water right has been inserted for this reservoir / system
            
    f2.close()
    os.remove(path1+'midinput.dat')
    os.rename(path1+'midinput1.dat', path1+'midinput.dat')    
    return
    

def midinput_alter(path1, wr, div):     # wamfile+'.dat', wr to be altered
    # prepare midinput.dat for wam
    f2=open(path1+'midinput1.dat','w')
    f1=open(path1+'midinput.dat','r')
    lines=f1.readlines()
    for line in lines:
        y0=line[:2]
        if y0!='**':
            if y0=='WR' and wr in line:             
                #print 'line1= ',line
                y1=line[:8]
                y2='{:>8.0f}'.format(float(div))
                y3=line[16:]
                line=y1+y2+y3
            f2.write(line)

    f1.close()
    f2.close()
    os.remove(path1+'midinput.dat')
    os.rename(path1+'midinput1.dat', path1+'midinput.dat')    
    return


def run_sim_table(path1):     # return maximum shortage
    # Run WAM, check Table output, detect and return maximum shortage
    file_list = []   
    os.system(path1+'runwam.bat')
    
    f1=open(path1+'tab.TOU','r')  # Table output file
    lines=open(path1+'tab.TOU','r').readlines()
    max=0.0
    for line in lines:
        x1=line[:2]
        if x1=='19': 
            x2=line[110:124]
            x2=float(x2)
            if x2>max: max=x2
    f1.close()
    return max
    

def extra_yields(filenew,file_extra):    # output extra yield
    f1=open(filenew,'r')
    lines=f1.readlines()
    f1.close()
    f2=open(file_extra,'w')
    
    y1='EXTRA_YLD'
    y2='Firm Yield='
    for line in lines:
        z1=line.find(y1)
        z2=line.find(y2)
        if z1!=-1 and z2!=-1:      # found the extra_yld output line
            f2.write(line)
    
    f2.close()
    return


def basin_fy (path1, path2, path3, wamfile, names, extra, basinwidefy, storage_set, percent): # calculate total firm yield of all reservoirs in the basin

    #fmt=dtype([('res','S6'),('cpid','S6'),('div','S8'),('pdate','S8'),('wrid','S16')])
    #wr_all=empty((20,5),dtype=fmt)
    fy=empty((100),dtype=float)
    fy.fill(0.0)

    # Call ini_files_2 to create midinputs.eva and midinput.inf/flo files for each new hydrology
    ini_files_2(path1, path2, wamfile, basinwidefy)
    #---------------------------------------------------------

    # Open firm yield computation output file ##############
    for fname in os.listdir(path3):
        if fname.startswith('fyout.out'): os.remove(path3+fname)       
    f6=open(path3+'fyout.out','w')
    #---------------------------------------------------------

#    ### Open input.dat file and read WAM file name and reservoirs names #############
#    f3=open(path1+'input.dat','r')   # list all reservoirs for firm yield compntation in a line with 6 spaces for each reservoir name
#    names=f3.readlines()     
#    wamfile=names[0]      # WAM file root name
#    wamfile=wamfile.strip()
#    names=names[1:]     # name(s) of the reservoir(s) for which to compute FY
    fybasin0=0.0
    fybasin=''
    fybasin_extra=''
    print names

    # #############################################################
    # Begin reservoir loop:
    # #############################################################
    for name in names:  
    #1. Find all wrs directly associated with the wanted reservoirs
    #--------------------------------------------------------------

    # Call ini_storage to create wamfile.dat from wamfile_ori.dat for each new reservoir
        if storage_set>0: ini_storage(path2, wamfile, name, percent)
    #---------------------------------------------------------
    
    # Call ini_files_3 to create midinput.dat for each new trial-and-error iteration
        ini_files_3(path1, path2, wamfile, basinwidefy)
    #---------------------------------------------------------

    # Call midinput_add to add Extra_YLD right for each reservoir / system
        if extra==1:
            midinput_add(path1, name)
    #---------------------------------------------------------------------
        wr_all=empty((100,5),dtype='S16')
        wr_all.fill('')
        name=name.strip()
        print 'Begin for ',name
        f6.write('Begin for '+name+'\n')
        f1=open(path1+'midinput.dat','r')   # wam input file of .dat
        lines=reversed(f1.readlines())
        n=0
        insert='yes'
        flag1=0
        tfy=0.0

        for line in lines:
            x=line[:2]
            if x=='WS'and flag1==0:
                y=line[2:8]
                z=name.find(y)
                if z!=-1:   
                    flag1=1
                    
            if x=='WR'and flag1==1:
                xr=line[8:16]   # diversion
                xr=xr.strip()   # only record wrs that have diversions > 0:
                if xr=='':
                    xr='0.0'
                if float(xr)>=1.0:     #a non-zero diversion water right                            
                    wr_all[n][1]=line[2:8]     # cpid
                    wr_all[n][2]=line[8:16]    # diversion
                    wr_all[n][3]=line[24:32]   # pri-date
                    wr_all[n][4]=line[64:80]   # wrid
                    n+=1
                flag1=0
            if x=='IF'and flag1==1:
                flag1=0
        
        f1.close()

        bu=0
        lines=reversed(open(path1+'midinput.dat','r').readlines())
        for line in lines:  # find all wrs backed by the wanted reservoirs
            x1=line[:2]
            x2=line[32:40]
            x2=x2.strip()   
            if bu==0 and x1=='BU': bu=1
            if bu==0 and x1=='SO' and x2=='BACKUP': bu=1
            
            if x1=='WR' and bu==2: 
                x=line[2:8]     # cpid
                while j<n:
                    if x!=wr_all[j][1]:
                        wr_all[n][0]='BACKED'
                        wr_all[n][1]=line[2:8]     # cpid
                        wr_all[n][2]=line[8:16]    # diversion
                        wr_all[n][3]=line[24:32]   # pri-date
                        wr_all[n][4]=line[64:80]   # wrid
                        bu=0
                        #print n, bu, y, wr_all[n][4]
                        n+=1
                    j+=1

            if x1=='WR' and bu==1:
                j=0
                y=line[64:80]
                while j<n:
                    bu=0
                    z=wr_all[j][4].find(y)
                    if z!=-1: 
                        bu=2
                        j=n
                    j+=1
                       
        f1.close()
        wr_all=wr_all[0:n]
        #wr_all=array(wr_all)
        print wr_all
        print 'No. of Water Rights Associated with this reservoir: ', n
        f6.write('No. of Water Rights Associated with this reservoir: '+str(n)+'\n')

    #2. Sort wrs by pri-dates
    #------------------------
        wr_all=sorted(wr_all, key=lambda wr_all: wr_all[3])
        i=0
        while i<n:
            f6.write(wr_all[i])
            f6.write('\n')
            i+=1
        
        print wr_all

    #3. Create a table input file for all rights
    #--------------------------------------------
        f1=open(path1+'tab.TIN','w')
        f1.write('TITL \n')
        i=0
        while i<n:
            x=wr_all[i][4].strip()
            f1.write('2SWR   0   1'+x.rjust(16,' ')+'\n')
            i+=1
        f1.write('ENDF \n')
        f1.close()

    #4. Firm Yield Run
    #-----------------------
    #   Method: 3-step approach.
    #
    #   4.1 Detecting the WR that causes shortage
    #   Run all n WRs with permitted div, if shoratge>0, run n-1 WRs,...,n-i WRs
    #   and on until shortage=0. Then the ith WR is the one that causes shortage.
    #  
    #   For the WRs that are senior to ith WR, their FYs (FY(1), FY(2),...,FY(n-i))
    #   equal to their permitted divs
    #
    #   4.2 Cumpute FY for all WRs after ith: FY(i), FY(i+1), ...,FY(n).
    #
    #   4.3 Total FY = sum of all FYs, from FY(1) to FY(n)
    #==========================================================
    #
    #  4.1
    #  Initial Trial
        print ' /////// Initial Run ///////////'
        km=n-1
        k=km
        kth=km
        flag=1
        tfy=0.0
        max=run_sim_table(path1)
        if max==0.0: 
            print 'max= ', max, 'All permitted diversions 100% firm'
            kth=n
            flag=0    # total firm yield (tfy) is found
        
        while k>=0 and flag==1:  # starting the test from k-1
            print ' ========= Search for Trouble maker =========='
            wr=wr_all[k][4]
            xx=wr_all[k][2].strip()
            if xx=='': wr_all[k][2]='0.000000'
            print wr_all[k][2]
            x=int(float(wr_all[k][2]))
            if x==0:    # if permitted diversion=0, move up to next wr
                fy[k]=1.0*x
                k=k-1
            else:            
                div=0.0
                # Call midinput_alter to change diversion ------------------
                midinput_alter(path1, wr, div)
                # Call run_sim_table to re-run sim and table with changed diversion --------------
                max=run_sim_table(path1)
                if max==0.0:      # found the trouble maker (kth)!
                    kth=k
                    flag=2
                    print 'The trouble maker is water right #',kth
                    f6.write('The trouble maker is WR #'+str(kth)+'\n')
                if max>0.0: 
                    if k==0: 
                        print 'All div set to zero and shortage still >0'
                        kth=k
                        flag=3
                        f6.write('All div set to zero and shortage still>0 for '+wr+'\n')
                    k=k-1
        
        #  Total FY for up to (but not including) kth WR:
        i=0
        print 'Starting simulation for the '+str(kth)+'th water right'
        while i<kth:
            x=wr_all[i][2].strip()
            print x
            if x=='': 
                x='0.0'
                print x
            fy[i]=float(x)
            tfy=tfy+fy[i]
            print i, tfy
            i+=1
        #
        #  4.2
        if kth<n:   # When all div are firm, the kth is set to n so no need to iterate
            min=10.0    # minimum delta and meaningful minimal diversion
            wr=wr_all[kth][4]
            divmax=float(wr_all[kth][2])
            divmin=0.0
            div=divmax
            change=1
            rate=0.5
        while flag==2:  # compute firm yield for trouble maker
            print ' ********** Iteration Runs ************'
            dd=(divmax-divmin)
            if dd<=min:
                if kth==km: # found fy for last WR as well as tfy. End.
                    fy[kth]=divmin
                    tfy=tfy+fy[kth]
                    flag=0
                if kth<km: # found fy for kth WR; move on to next WR
                    midinput_alter(path1, wr, divmin) #update midinput.dat with found fy for kth WR before moving on to next WR
                    fy[kth]=divmin
                    tfy=tfy+fy[kth]
                    kth=kth+1
                    wr=wr_all[kth][4]
                    divmax=float(wr_all[kth][2])
                    divmin=0.0
                    dd=(divmax-divmin)
            
            if flag!=0:
                div=divmin+0.618*dd   # faster if converging to non-zero final (searching towards right)
                if change==1: div=divmax-0.8*dd     # faster if converging to zero-final (searching towards left)
                midinput_alter(path1, wr, div)
                max=run_sim_table(path1)  
                #rate=max/div
                print str(kth)+wr+': '+str(int(divmin))+'   '+str(int(div))+'   '+str(int(divmax))+'   '+str(int(max))
                f6.write('Iteration '+wr+': '+str(int(divmin))+'   '+str(int(div))+'   '+str(int(divmax))+'   '+str(int(max))+'\n')
                if max==0.0: 
                    divmin=div
                    change=0
                if max>0.0: 
                    divmax=div
                    change=1

        i=0
        while i<n:
            wr=wr_all[i][4]
            div=wr_all[i][2]
            f6.write(str(i)+' '+name+':'+wr+'"s Firm Yield=, '+str(int(fy[i]))+', out of its permit of '+div+'\n')
            extra_yld=''
            if wr.find('EXTRA_YLD')!=-1:
                extra_yld=str(int(fy[i]))
                
            i+=1

        f6.write(name+'"s Total Firm Yield=, '+str(int(tfy))+'\n')

        print 'End for ',name
        f6.write('End for '+name+'\n')
        fybasin0=fybasin0+tfy
        fybasin=fybasin+str(int(tfy))+','
        if extra==1: fybasin_extra=fybasin_extra+extra_yld+','
            
        print 'fybasin= ',fybasin
        
    f3.close()
    f6.close()
    if extra==1: 
        fybasin=fybasin+str(int(fybasin0))+',Extra Yields:,'+fybasin_extra
    else:
        fybasin=fybasin+str(int(fybasin0))
    return fybasin


# #########################################################################
# Main program starts here:
# You need to set/change 5 prameters: 
# (1) give basin name,
# (2) do you want basin-wide firm yield or individual firm yield (set basinwidefy = yes or no)? 
# (3) do you want to do extra yield (extra=1 or 0)?
# (4) do you want to set initial reservoir storage (0: no, 1: individual reservoir(s), 2: all reservoirs)
#     you also need to set storage increase steps. The storage then will be increased by 100%/steps every time until 100%
# (5) set loop number (mm)
# #########################################################################
import string
import subprocess
import os
import shutil
from operator import attrgetter, itemgetter
from numpy import *     #include array, dtype, empty
import fileinput 
import sys
# basin_fy function needs all above imports

# Parameter 1:
basin='Brazos'
root='/share/work/yyang/wam/'+basin

pwd=os.getcwd()
print 'In FY_Unix.py.  Cuurent path is '+pwd
path1=pwd+'/'
path2=root+'/Data/'
path3=path1

f4=open(path1+'sim.in.txt','r')   # run number
run_num=f4.readlines()
run_num=int(run_num[0])
print run_num
f4.close()

basinwidefy='no'
# Open input.dat for basic information (WAM root, reservoir names) ######
f3=open(path1+'input.dat','r')   # list all reservoirs for firm yield compntation in a line with 6 spaces for each reservoir name
names=f3.readlines()     
wamfile=names[0]        # WAM file root name; upper / loweer case sensetive
wamfile=wamfile.strip()
names=names[1:]     # name(s) of the reservoir(s) for which to compute FY
f3.close()
#---------------------------------------------------------------------------

# Call ini_files_1 to create midinput.dis, 1 batch and 2 misc files
ini_files_1(path1, path2, wamfile)
#---------------------------------------------------------------------------

# Parameter 3:
# If you want examine extra yield, set extra=1, otherwise, extra=0
extra=1
#--------------------------------------------------------------------------------------------

# Parameter 4:
# do you want to set initial reservoir storage? if so set storage_set=1; otherwise 0;
# also set storage increase steps (if 5 steps, the initial storage will start from 0% and go up 100%/5=20% every time: 0, 20, 40, 60, 80, and 100%)
storage_set=0
steps=10
if storage_set>0:
    for fname in os.listdir(path2):
        if fname.startswith(wamfile+'_ori.dat'): os.remove(path2+fname)
    os.rename(path2+wamfile+'.dat', path2+wamfile+'_ori.dat')
#--------------------------------------------------------------------------------------------

#fybasintotal=0.0
fybasintotal=''
file_output=path3+wamfile+'_basin_fy.dat'
if extra==1:
    file_output=path3+wamfile+'_basin_extra_fy.dat'
f10=open(file_output,'w')    # all basin total fy values
f10.close()
   
# Parameter 5:
i=int(run_num)
mm=i+1
while i<mm:    
    f10=open(file_output,'r+')    # all basin total fy values
    lines=f10.readlines()
    y=str(i)
    print 'Running::::::::::::::::::::::::::::::::',i,'th file'
    for fname in os.listdir(path3):
        if fname.startswith('fyout'): os.remove(path3+fname)

# prepare .eva, .inf, and .fad files
    root1=path2+wamfile+'_'+y
    rootnew=path2+wamfile
    for fname in os.listdir(path2):
        if fname.startswith(wamfile+'.eva'): 
            os.remove(path2+fname)
        if fname.startswith(wamfile+'.inf'):             
            os.remove(path2+fname)
            ext='.inf'
            #print ext
        if fname.startswith(wamfile+'.flo'): 
            os.remove(path2+fname)
            ext='.flo'
            #print ext
    # If cannot find ext, check the input.dat to make sure upper/lower case match for wamfile name    
    file1=root1+'.eva'
    filenew=rootnew+'.eva'
    shutil.copy(file1, filenew)
    
    #file1=root1+'.fad'
    #filenew=rootnew+'.fad'
    #shutil.copy(file1, filenew)

    ext='.inf'
    file1=root1+ext 
    filenew=rootnew+ext
    shutil.copy(file1, filenew)
    
# Call basin_fy function --------------------------------
    j=0
    jm=int(storage_set*steps+1)
    #jm=int(storage_set*steps)
    while j<jm:   # initial storage loop
        percent=j*int(100/steps)
        z=str(percent)
        fybasintotal=basin_fy(path1, path2, path3,wamfile, names, extra, basinwidefy, storage_set, percent)
# Rename fyout.out into numbered output files-------------------------------------
        file1=path3+'fyout.out'
        filenew=path3+wamfile+'_'+y+'_'+z+'.out'
        for fname in os.listdir(path3):
            if fname.startswith(wamfile+'_'+y+'_'+z+'.out'): os.remove(path3+fname)
        os.rename(file1, filenew)
        f10.write(y+', When initial storage is '+z+'%, FYs are '+fybasintotal+'\n')
        j+=1
    f10.close()

# Output extra yields:
    if extra==1:
        file_extra=path3+wamfile+'_extra_yld_'+y+'.out'
        extra_yields(filenew, file_extra)
    
    i+=1

