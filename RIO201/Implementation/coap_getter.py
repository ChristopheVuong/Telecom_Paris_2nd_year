# -*- coding: utf-8 -*-
"""
Created on Tue Nov  6 12:41:58 2018

@author: Christophe V
"""

import subprocess
import time

"""
Store the address of coap servers 
from another program
"""
address = []
number_nodes = 10


def coap_receiver(i): 
    number_requests = 100
    period = 0.05
    #get the i-th adress    
    i = 0
    command0 = "coap get "
    """
    the i-th address
    coap_server = "coap://[ipv6 address]"
    port = ":2555"
    """
    output1 = "/sensors/accel"
    output2 = "sensors/light"

    string = command0
    while i <= number_requests:
        a = time.time()
        result = subprocess.check_output(string, shell = True)
        b = time.time()
        c = b-a #delays
        #write c, i and string in a file with titles
        print(result) 
        i = i + 1
       

for i in range(number_nodes):
    t = Thread(target = coap_receiver, args=(i,))
    t.start()