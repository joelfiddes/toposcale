#!/usr/bin/python
from ecmwfapi import ECMWFDataServer
server = ECMWFDataServer()
server.retrieve({
'dataset' : 'interim',
'date'    : '20121101/to/20121105',
'stream'	 : 'oper',
'time'    : '00/06/12/18',
'grid'    : '0.75/0.75',
'step'    : '0',
'levtype' : 'pl',
'type'    : 'an',
'class'   : 'ei',
'param'   : '132',
'area'    : '31.925/77.575/31.825/77.675',
'levelist': '500/650/775/850/925/1000',
'target'  : 'vpl.grb'
    })
