# Up-Down-Motor-Control

Testing the knowledge acquired in Assembly (RISC architecture).

All learning processes require practical exercises, especially when it comes from a self-taught.

The challenge was to make a device to raise and lower the electric shutters on my bedroom window. So, I start by looking for material available in my workshop in order to make the execution of my project as cheap as possible. I found two optocouplers MOC3011, two triac BT136 plus some resistors and necessary capacitors, so I only needed to buy the micro-controller, having chosen the PIC16F627, a capacitor for the power circuit and a low-value ceramic resistance to measure the load current in the circuit.

After designing and testing the schematic in an electronic circuit simulator, making the printed circuit board and soldering all the electronic elements, it was time for the software.

This should measure the current in the circuit so as to be able to switch off the triac when the limit switch is activated opening the circuit, and providing protection against short circuit as well as detecting mechanical arrest in the motor.
    It should distinguish a short press from a long press on the up or down button. A short press should go all the way until activate the limit switch. ´With a long press it should end the action at press ending.

Provide a button to enter in configuration mode. This button will initialize a sequence to detect and store in memory the keys from IR command that will trigger the up and down commands, identifyed by a blinking led.


IR protocols:
   - RC5  (Phlips)
   - RC6  (Philips)
   - SIRC (Sony)
   - PWC  (Nec)
   - (Grundig)
   - (Samsung) 

c![Screenshot](Images/005.png)
![Screenshot](Images/002.png)
![Screenshot](Images/003.png)
