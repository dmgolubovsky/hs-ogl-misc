# Introduction #

Virtual device drivers use 9P2000 (as encoded by the NineP package) to communicate with the IO layer. Certain deviations are assumed for drivers' implementation of 9P2000 messages exchange.

  * Drivers always operate synchronously. A calling thread always blocks after sending a message to a driver until response is returned.

  * Messages going between drivers and the IO layer never fragment: one request always yields one response.

  * A special FID value (NOFID, complement of 0) if used with the clunk request should clunk all`*` FIDs the device has currently active (including FIDs for tree roots). This form of clunk is not expected to be used in normal course of application action; rather it is used by each thread's toplevel exception handler to clean up connected devices.

  * Local drivers should not require authentication, that is, respond with error to the auth message, and accept NOFID as afid for attach message along with empty user name without error.

  * When reading from a directory, all Stat structures that can be obtained from the directory will be returned. Offset can be only 0, and length is ignored.

  * Local device driver may instead of returning an error response throw an error when its state needs not to be updated (that is, along with error message its current state would be returned).



---

`*` if a driver serves multiple threads and takes into account which thread uses which FIDs, clunk should close all FIDs used by the calling thread.