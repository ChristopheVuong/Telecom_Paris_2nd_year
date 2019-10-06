# -*- coding: utf-8 -*-
"""
Created on Mon Nov 12 16:47:04 2018

@author: cvuong
"""
"""
This code provides with http clients for various types of data sent
"""

import subprocess
import time
from threading import Thread, RLock
import numpy as np
  

log_str = "http-thread-log.txt" 

"""
Store the address of http servers 
"""
address = ["2001:660:4403:498::1757","2001:660:4403:498::2559" ,"2001:660:4403:498::2458"]
number_nodes = 3

delay = 0.05
"""create a shift with several sleep periods, allow several nodes to work in the flow""" 
period = [delay, 2*delay, 3*delay]

"""for statistics"""
counter = np.zeros(number_nodes)
transmitted =  np.zeros(number_nodes)
number_requests = 15

command0 = "lynx -dump "
#command1 = "lynx -dump URL > filename"

#threads
lock = RLock()
list_threads = []

class http(Thread):
    """Thread for a http request by the i-th node"""
    def __init__(self,file,number):
        Thread.__init__(self)
        self.file = file
        self.number = number

    def run(self):
        """Code"""   
        file = self.file
        file.write("\n")
        """Indicate the period associated with each node (just for a better reading of the log""" 
        file.write("HTTP period : " + str(self.number) + ": " + str(period[self.number]) + "\n")
        file.write("\n")

        while counter[self.number] < number_requests:
            with lock:
                http_server = "http://[" + address[self.number] + "]"
                a = time.time()
                string = command0 + http_server
                result = subprocess.check_output(string, shell = True)
                b = time.time()
                c = b-a #delays
                """if there is an error at subprocess, an error will be raised by the system"""
                """to make sure the sensor node send something (measures)"""
                if result:
                    file.write(str(self.number) + "%" + str(counter[self.number]) + ") " + " Time : " + str(c) +"\n")
                    transmitted[self.number] += 1
                counter[self.number] += 1
                time.sleep(period[self.number])



file = open(log_str,"a")        
# Create threads
"""Create threads"""
for i in range(number_nodes):
    thread = http(file,i);
    list_threads.append(thread)

"""Start threads"""
for i in range(number_nodes):    
    list_threads[i].start()
"""Wait until the threads end"""
for i in range(number_nodes):    
    list_threads[i].join()
    

for k in range(number_nodes):
    file.write("\n")
    file.write("Transmission ratio (requests) :" + "\n")
    file.write("Node " + str(k) + ": Number of requests done = " + str(counter[k]) + "; " + str(100 * transmitted[k]/counter[k]) + " %" + "\n")
    file.write("End experiment" + "\n")
    
print("HTTP done") 
file.close()