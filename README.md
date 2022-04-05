# GNU Linux Desktop System for IPhone &amp; IPad

![QMole Banner](artwork/qmole.png)

[See also: QMole Cydia Reposity on Github](https://github.com/chriskmanx/qmole-packages)

[See also: iPyhton Notebook on iPhone](https://chriskohlhepp.wordpress.com/linux-on-iphone/ipython-notebook-on-iphone/)

[See also: QMole Linux Desktop Manual](https://chriskohlhepp.wordpress.com/linux-on-iphone/qmole-linux-desktop-for-ios-manual/)

## What is Qmole?

### 1) It’s a "Smart Routing Tiling Window Manager" for touch screen tablets and smart phones.

[Graphical Linux Applications, Native on iPhone](https://chriskohlhepp.wordpress.com/linux-on-iphone/)

42 million lines of Linux code on iOS. That’s kind of a world’s first !

![Testimonials](artwork/notepad.png)


### 2) It’s a compatibility container

QMole allows iPhones and iPads to run graphical Linux applications. It enables touch-screen operation of Linux applications executing together with native iOS applications on a self contained Linux style desktop. This desktop introduces automatic window management for the touch screen and enables full access to the data on your device.

## QMole Architecture

![QMole Archiecture](artwork/qmole-inux-architecture.png)


QMole is a hybrid. It is not a virtualization solution like VMWare or VirtualBox. Neither is it an operating system. It requires no new API of application developers, as does iOS. QMole sandwiches applications in standard Linux abstraction on one side and provides touch screen abstractions and window management on the other side. This provides re-usability of Linux applications. In architectural terms, QMole occupies a space somewhere between BSD and Linux, but based on the Darwin kernel.

QMole uses the excellent [Herbstluft Window Manager](https://www.herbstluftwm.org). The window managers [Fluxbox](http://fluxbox.org), [i3](https://i3wm.org), and [Awesome](https://awesomewm.org) are also supported. 

QMole is designed and authored by [Chris Kohlhepp](https://chriskohlhepp.wordpress.com) - [LinkedIn Profile](https://www.linkedin.com/in/chriskohlhepp/) and licensed open-source under the GNU GENERAL PUBLIC LICENSE. A special mention goes to Florian Engel of Cologne, Germany, for being the maintainer and scribe of QMole for the last year and a half. Thank you Florian. Thank you also to our beta testers Kyle Serbov,  Jimmy Huy & Tejas Desai.

## The Story

![Flying Mole Banner](artwork/qmolefly.png)

So here is a small story. Some years ago my wife gave me an "generation one" iPad as a present - my first touch screen device. Since the iPad had a solid state drive, it would outlast, on a full battery charge, any notebook with a conventional hard drive that I had at the time. I could, and did, fly from Sydney to Los Angeles and keep using my iPad. Yet my initial euphoria was short lived. I quickly found myself in Apple's "Walled Garden." I had to "buy to try," and more often than not, apps failed to provide the feature depth and breadth of comparable applications on Microsoft Windows or Linux - even where the Windows or Linux competitors were entirely free. Frequently, I found myself buying multiple apps for the same purpose to arrive at something to my liking. Very early did I realise that if I wanted the functionality I sought, I would need to build it myself. Cydia already provided access to a host of GNU / Linux applications on the iPhone, but only terminal based. I wanted Java. I wanted a GTK desktop. I wanted the iPython notebook (to tun OCaml). I wanted a tiling window manager that routed my apps to virtual desktops on demand - load balancing for touch screen apps. And I wanted it on my iPhone. So I built it and I called it QMole. A mole because it subsumed what was there (the garden) and "Q" because I didn't want another "I" something.

Over the years, QMole served me well. I built everything on the iPhone and iPad. In due time, I had built almost an entire "Linux" distribution with some 170 Debian packages, comprising some 42 million lines of code . As time went on, Apple changed from 32 bit CPUs to 64 bit CPUs. The format for executables on iOS changed a number of times. Ultimately, I had to decide if I wanted to maintain my own Linux distribution or give "my Linux" back to whom it belongs: the community.

So today I have done just that. I released QMole open-source on Github. A separate Github repository holds the Debian packages which makes for a smaller download footprint.

Let it fly (hence the painting of the flying mole).

Happy hacking !

## Manifest

### QMOLEDEV

Primary source repository of *ported* applications, all built in-place on a first generation iPad. Configure and build scripts have been modified to work around peculiarities of iOS, such as needing signed binaries to prevent the kernel from killing off intermediate executables produced during configuration or compilation. This means no cross build system, e.g. Linux or Mac OSX is needed here. These are 32 bit builds. 


### QMOLEDEV64

Secondary source repository of *ported* systems, all built in-pace on an iPhone 5S. These are 64 bit builds. Primary application focus is a Swiss Army Knife of network tools that turns your iphone into a Wifi hotspot security tool. You will find network capture tcpdump, nmap and snort tools here. 

### XORG0 XORG1 XORG2

Primary source repositories of *ported* X11 (Unix Windowing) systems.

### qmole-msg
### qmole-probe
### qmolegui
### qmoledevice

This is the core of the QMole system itself, written predominantly in Objective-C and OCaml.  Objective-C accommodates the front-end of the system. OCaml accommodates the back-end of the system.

### scripts

Various helper and build scripts to assist with building the REPO

### 1qmole-screenshots

Screenshots showing QMole capabilities

### 2cydia-screenshots

Screenshots showing QMole installation via Cydia

### artwork

Various sketches, icons, and drawings - thanks to my dear wife. 

## Licence Disclaimer

QMole is hereby released open-source under the GPL. Previous releases were licensed closed-source QMole UK (this continues to apply to the existing binaries up to and including beta version 0.7). The reason for the closed-source release is that QMole has dependencies on screen viewer libraries which themselves are closed-source and licensed content. Adopters have 3 main choices:

1. Also license the screen viewer libraries; please contact [ABTO Software](http://remote-screen.com)
2. Adapt the GPL licence compatible [Google Code VNSea](https://code.google.com/archive/p/vnsea/)
3. Use RealVNC's viewer on the Apple APP Store directly; please refer to [VNC Viewer](https://itunes.apple.com/us/app/vnc-viewer/id352019548?mt=8)

Each approach as its merits. *The existing implementation is optimized for use with QMole including swipe control overlays (the scroll ribbon) and fine-tuned mouse control that make standard X11 Linux applications readily usable on a touch screen with limited screen real estate.* Google Code's VNSea has the benefit of being open source, but will need adapting for use with QMole. RealVNC's viewer is a turn-key solution, but will need to be separately launched as it cannot be directly integrated with QMole. All three merely need to form loopback connections to the local device, so are never used as "remote" viewers. Please refer to [QMole Linux Desktop Manual](https://chriskohlhepp.wordpress.com/linux-on-iphone/qmole-linux-desktop-for-ios-manual/) for notes on QMole's touch screen optimization.

If you require different licence terms, feel free to get in touch with me. 

## iOS Compatibility Notes

The last version of iOS for which I maintained QMole was iOS 8. Apple has changed binary formats a number of times since the initial release of iOS, moving from 32 bit to 64 bit formats in the process. The latest version of iOS requires a rebuild of the current sources but is otherwise expected to work in full. A "rooted" device is required in all cases.

## Android Compatibility Notes

It is expected that QMole would function on Android devices after appropriate recompilation of the sources. Packages like [Termux](https://termux.com) and [GNURoot](https://play.google.com/store/apps/details?id=champion.gnuroot&hl=en) are expected to be usable with QMole - even without "rooting" the device.

## A Complex System

QMole comprises some *42 million* lines of ported and original code. Most of the core components are written in either Objective-C or OCaml, with OCaml solutions rarely beyond a few hundred lines of code. Most of the ported Unix code is C and C++. QMole has been built "on device," either on iPhone or iPad.


## Happy Hacking!










