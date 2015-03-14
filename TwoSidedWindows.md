# General #

Similar idea for control placement, here: [Double Sided Windows](http://priorartdatabase.com/IPCOM/000160567#textpreview)

What's proposed here is different (not sure if it already was discussed anywhere). This may be similar to Tek4014 emulation in Xterm where either widget (text or graphics) may be made visible hiding another.

Each window is created with two sides: one to be used for text-based interaction (connects to the application's standard input and output), another to be used as a canvas (connects to application's virtual graphic oonsole). User may flip sides by some predefined mouse action or pre-assigned key on the keyboard.
Application may interleave IO actions with both sides; only those become immediately visible that apply to the currently exposed side.

Application may request user's attention to either side programmatically; in this case window frame changes color. Window side fliping on behalf of an application is not encouraged.

This approach allows the two interface modes to coexist in a single window without unneeded interference (unlike RIO where it is possible to produce text output while the window is showing graphics).