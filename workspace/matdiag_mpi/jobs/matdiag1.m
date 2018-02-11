% 
%   This script uses geodise toolbox to run simple wave model
%   on iceberg and submit the job to sun grid engine


JOBMANAGER=['grid-compute.leeds.ac.uk/jobmanager-pbs']
RSLstring=['&(executable=./matdiag_mpi
)(directory=proj/cppdev/workspace/matdiag_mpi
)(arguments="8" )(maxCpuTime=600 )(jobType=mpi )(count=4
)(environment=(NGSMODULES clusteruser) )(stdout=ex1.out )(stderr=ex1.err )']
jobhandle = gd_jobsubmit(RSLstring,JOBMANAGER);

