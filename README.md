# qmole
GNU Linux Desktop System for IPhone &amp; IPad

![QMole Banner](qmole.png)

[Cydia Reposity Packages Github Repository](https://github.com/chriskmanx/qmole-packages)

[iPyhton Notebook on iPhone](https://chriskohlhepp.wordpress.com/linux-on-iphone/ipython-notebook-on-iphone/)

[QMole Linux Desktop Manual](https://chriskohlhepp.wordpress.com/linux-on-iphone/qmole-linux-desktop-for-ios-manual/)

What is Qmole?

1) It’s a smart routing tiling window manager for tablets.


That’s kind of a world’s first !

![Testimonials](notepad.png)


2) It’s a compatibility container

QMole allows iPhones and iPads to run graphical Linux applications. It enables touch-screen operation of Linux applications executing together with native iOS applications on a self contained Linux style desktop. This desktop introduces automatic window management for the touch screen and enables full access to the data on your device.

QMole Architecture""

![QMole Archiecture](qmole-inux-architecture.png)


QMole is a hybrid. It is not a virtualization solution like VMWare or VirtualBox. Neither is it an operating system. It requires no new API of application developers, as does iOS. QMole sandwiches applications in standard Linux abstraction on one side and provides touch screen abstractions and window management on the other side. This provides re-usability of Linux applications. In architectural terms, QMole occupies a space somewhere between BSD and Linux, but based on the Darwin kernel. 

QMole is designed and authored by Chris Kohlhepp and licensed open source under the GNU GENERAL PUBLIC LICENSE.

![Flying Mole Banner](qmolefly.png)

## Licence Disclaimer

QMole is hereby released open-source under the GPL. Previous releases were licensed closed source (this continues to apply to the existing binaries up to an including beta version 0.7). The reason for the closed-source release is that QMole has dependencies on screen viewer libraries which themselves are closed-source and licensed content. Adopters have 3 main choices:

1. Also license the screen viewer libraries; please contact [ABTO Software](http://remote-screen.com)
2. Adapt the GPL licence compatible [Google Code VNSea]()https://code.google.com/archive/p/vnsea/)
3. Use RealVNC's viewer on the Apple APP Store directly. [VNC Viewer](https://itunes.apple.com/us/app/vnc-viewer/id352019548?mt=8)

Each approach as its merits. The existing implementation is optimized for use with QMole including swipe control overlays (the scroll ribbon), and fine tuned mouse control, that make standard X11 Linux applications readily usable on a touch screen with limited screen real estate. Google Code's VNSea has the benefit of being open source, but will need adapting for use with QMole. RealVNC's viewer is a turn-key solution. All three merely need to form loopback connections to the local device, so are never used as remote viewers.

## Compatibility Notes

The last version of iOS for which I maintained QMole was iOS 8. Apple has changes binary formats a number of times since the initial release of iOS, moving from 32 bit to 64 bit formats in the process. The latest version of iOS requires a rebuild of the current sources but is otherwise expected to work in full.








