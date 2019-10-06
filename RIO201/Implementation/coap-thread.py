# -*- coding: utf-8 -*-
"""
Created on Mon Nov 12 16:47:04 2018

@author: cvuong
"""

"""
This code provides with CoAP clients that can grab one or several measures according this scenario :
1) acceleration
2) light
3) light + gyros
4) light + gyros + magne
5) magne

The order can be changed, depending on the limitations of sensors in the nodes in term of response.
"""
import subprocess
import time
from threading import Thread, RLock
import numpy as np
   
   

log_str = "CoAP-thread-log.txt"

"""
Store the address of CoAP servers 
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

command0 = "coap get "
port = ":5683"   
output = ["/sensors/accel","sensors/light","sensors/gyros","/sensors/magne","/sensors/pressure"]    

#threads
lock = RLock()
list_threads = []


class coap(Thread):
    """Thread for a CoAP request by the i-th node"""
    def __init__(self,file,number):
        Thread.__init__(self)
        self.file = file
        self.number = number
                       
            
    def run(self):
        """Code"""   
        file = self.file
        file.write("\n")
        """Indicate the period associated with each node (just for a better reading of the log""" 
        file.write("CoAP period : " + str(self.number) + ": " + str(period[self.number]) + "\n")
        file.write("\n")


        while counter[self.number] < number_requests:
            """Lock to run this entire section in once"""
            with lock:
                coap_server = "coap://[" + address[self.number] +"] "
                a = time.time()
                if self.number == 0:
                    string = command0 + coap_server + port + output[0]
                    result = subprocess.check_output(string, shell = True)
                    """Could have done try-catch but many cases too handles -> heavy code"""
                if self.number == 1:
                    string = command0 + coap_server + port + output[1]
                    result = subprocess.check_output(string, shell = True)
                if self.number == 2:
                    string1 = command0 + coap_server + port + output[1]
                    string2 = command0 + coap_server + port + output[2]
                    result =  (subprocess.check_output(string1, shell = True), subprocess.check_output(string2, shell = True))
                if self.number == 3:
                    """Increase the number of measures sent"""
                    string1 = command0 + coap_server + port + output[2]
                    string2 = command0 + coap_server + port + output[3]
                    string3 = command0 + coap_server + port + output[4]
                    result = (subprocess.check_output(string1, shell = True), subprocess.check_output(string2, shell = True), subprocess.check_output(string3, shell = True))
                if self.number > 3:
                    string = command0 + coap_server + port + output[3]
                    result = subprocess.check_output(string, shell = True)
                #string = command0 + coap_server + port + output[self.number]
                #result = subprocess.check_output(string, shell = True)
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
     
"""Create threads"""
for i in range(number_nodes):
    thread = coap(file,i);
    list_threads.append(thread)

"""Start threads"""
for i in range(number_nodes):    
    list_threads[i].start()
"""Wait until the threads end"""
for i in range(number_nodes):    
    list_threads[i].join()
    
for k in range(number_nodes):
    file.write("\n")
    file.write("Transmission ratio (requests): \n")
    file.write("Node " + str(k) + ": Number of requests done = " + str(counter[k]) + "; " + str(100 * transmitted[k]/counter[k]) + " %" + "\n")
    file.write("End experiment\n")
    
print("CoAP done")
file.close()