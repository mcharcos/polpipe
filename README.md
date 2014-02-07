polpipe
=======

Pipeline of optical polarization data from Mt Lemmon Observatory Facility.

REQUIREMENTS
============

IDL license
ksh 
Astro and Coyote libraries

INSTALLATION
============

Download the code from https://github.com/mcharcos/polpipe

Call ./install_mlof . path/to/astro/library/directory path/to/coyote/library/directory
This will set the mlofidlpath with the correct path. This script sets the environement each time a script is called.

USAGE
=====

The main pipeline is called: pipemlof. Read the usage manual for details on how to call it (pipemlof --man)
This script calls a serie of scripts that process a specific step of the pipeline. For each step, pipemlof allows to skip it, end at a it or run it in interactive or debug mode. 

To know more about how to use a specific routine read the usage manual. For example, dophot --man

