//
//  ViewController.m
//

#import "RemoteScreenViewController.h"
#import "RemoteScreenView.h"
#import <sys/sysctl.h>
#import <objc/runtime.h>
#include <stdio.h>
#include <unistd.h>



@implementation RemoteScreenViewController

@synthesize server=_server;

// https://discussions.apple.com/thread/4997560
- (BOOL)shouldAutorotate
{
    return NO;
}
- (NSUInteger)supportedInterfaceOrientations
{
    return UIInterfaceOrientationMaskPortrait;
}



- (NSString *)pathForTemporaryFileWithPrefix:(NSString *)prefix
{
    NSString *  result;
    CFUUIDRef   uuid;
    CFStringRef uuidStr;
    
    uuid = CFUUIDCreate(NULL);
    assert(uuid != NULL);
    
    uuidStr = CFUUIDCreateString(NULL, uuid);
    assert(uuidStr != NULL);
    
    result = [NSTemporaryDirectory() stringByAppendingPathComponent:[NSString stringWithFormat:@"%@-%@", prefix, uuidStr]];
    assert(result != nil);
    
    CFRelease(uuidStr);
    CFRelease(uuid);
    
    return result;
}

- (void)dealloc
{
    [_server release];
    [super dealloc];
}

- (void)centreMouse
{
    mouseX  = x11Width / 2;
    mouseY  = x11Height / 2;
    CGPoint here = CGPointMake(mouseX,mouseY);
    [_qmoleScreenView.session sendMouseMove:here ];
    
    /*NSString *cmd2 = [NSString stringWithFormat:@"echo center mouse at location X: %f Y:%f  >> /tmp/qmole.log &",
                      here.x,
                      here.y
                      ];
    system([cmd2 cStringUsingEncoding:NSASCIIStringEncoding]);*/
    
}

- (void)detectScreenSize
{
    NSString *configuration = [[[NSBundle mainBundle] infoDictionary] objectForKey:@"Configuration"];

    if ([configuration isEqualToString:@"Debug"]) system("echo detectScreenSize start >> /tmp/qmoletrace.log");
    
    /* https://www.theiphonewiki.com/wiki/Models */
    size_t size;
    sysctlbyname("hw.machine", NULL, &size, NULL, 0);
    char *machine = malloc(size);
    sysctlbyname("hw.machine", machine, &size, NULL, 0);
    NSString *platform = [NSString stringWithCString:machine encoding:NSUTF8StringEncoding];
   
    _isSmallScreen = NO;
    _isIpad = NO;
    _iKeyboardUpY = 0;
    _iKeyboardDownY = 0;
    
    NSString *cmd = [NSString stringWithFormat:@"echo Detected machine type %@ >> /tmp/qmole.log &",
     platform
     ];
    system([cmd cStringUsingEncoding:NSASCIIStringEncoding]);
    
    
    if (strcmp(machine,"iPad1,1") == 0){    /* Ipad 1 */
        x11Width = 768;
        x11Height = 1024;
        _isSmallScreen = YES;
        _isIpad = YES;
        _iKeyboardDownY = 145;
        _iKeyboardUpY = 315;
    } else if  ((strcmp(machine,"iPad2,1") == 0) ||  /* Ipad 2 */
                (strcmp(machine,"iPad2,2") == 0) ||
                (strcmp(machine,"iPad2,3") == 0) ||
                (strcmp(machine,"iPad2,4") == 0)) {
        x11Width = 768;
        x11Height = 1024;
        _isSmallScreen = YES;
         _isIpad = YES;
        _iKeyboardDownY = 145;
        _iKeyboardUpY = 315;
    } else if  ((strcmp(machine,"iPad3,1") == 0) ||  /* Ipad 3 */
                (strcmp(machine,"iPad3,2") == 0) ||
                (strcmp(machine,"iPad3,3") == 0)) {
        x11Width = 768;
        x11Height = 1024;
        _isSmallScreen = YES;
         _isIpad = YES;
        _iKeyboardDownY = 145;
        _iKeyboardUpY = 315;
    } else if  ((strcmp(machine,"iPad3,4") == 0) ||  /* Ipad 4 */
                (strcmp(machine,"iPad3,5") == 0) ||
                (strcmp(machine,"iPad3,6") == 0)) {
        x11Width = 768;
        x11Height = 1024;
        _isSmallScreen = YES;
         _isIpad = YES;
        _iKeyboardDownY = 145;
        _iKeyboardUpY = 315;
    }  else if  ((strcmp(machine,"iPad4,1") == 0) ||  /* Ipad Air */
                 (strcmp(machine,"iPad4,2") == 0) ||
                 (strcmp(machine,"iPad4,3") == 0)) {
        x11Width = 1536;
        x11Height = 2048;
        _isSmallScreen = NO;
         _isIpad = YES;
        _iKeyboardDownY = 145;
        _iKeyboardUpY = 315;
    }  else if  ((strcmp(machine,"iPad5,3") == 0) ||  /* Ipad Air2 */
                 (strcmp(machine,"iPad5,4") == 0)) {
        x11Width = 768;
        x11Height = 1024;
        _isSmallScreen = YES;
         _isIpad = YES;
        _iKeyboardDownY = 145;
        _iKeyboardUpY = 315;
    }  else if  ((strcmp(machine,"iPad2,5") == 0) ||  /* Ipad Mini 1G */
                 (strcmp(machine,"iPad2,6") == 0) ||
                 (strcmp(machine,"iPad2,7") == 0)) {
        x11Width = 768;
        x11Height = 1024;
        _isSmallScreen = YES;
         _isIpad = YES;
        _iKeyboardDownY = 145;
        _iKeyboardUpY = 315;
    }  else if  ((strcmp(machine,"iPad4,4") == 0) ||  /* Ipad Mini 2 */
                 (strcmp(machine,"iPad4,5") == 0) ||
                 (strcmp(machine,"iPad4,6") == 0)) {
        x11Width = 768;
        x11Height = 1024;
        _isSmallScreen = YES;
         _isIpad = YES;
        _iKeyboardDownY = 145;
        _iKeyboardUpY = 315;
    } else if  ((strcmp(machine,"iPad4,7") == 0) ||  /* Ipad Mini 3 */
                (strcmp(machine,"iPad4,8") == 0) ||
                (strcmp(machine,"iPad4,9") == 0)) {
        x11Width = 768;
        x11Height = 1024;
        _isSmallScreen = YES;
         _isIpad = YES;
        _iKeyboardDownY = 145;
        _iKeyboardUpY = 315;
    } else if (strcmp(machine,"iPhone1,1") == 0){    /* Iphone 1 */
        x11Width = 768;
        x11Height = 1024;
        _isSmallScreen = YES;
        _iKeyboardDownY = 145;
        _iKeyboardUpY = 315;
    } else if (strcmp(machine,"iPhone1,2") == 0){    /* Iphone 3G */
        x11Width = 768;
        x11Height = 1024;
        _isSmallScreen = YES;
         _isIpad = NO;
        _iKeyboardDownY = 145;
        _iKeyboardUpY = 315;
    } else if (strcmp(machine,"iPhone2,1") == 0){    /* Iphone 3GS */
        x11Width = 768;
        x11Height = 1024;
        _isSmallScreen = YES;
        _isIpad = NO;
        _iKeyboardDownY = 145;
        _iKeyboardUpY = 315;
    } else if (strcmp(machine,"iPhone2,1") == 0){    /* Iphone 3GS */
        x11Width = 768;
        x11Height = 1024;
        _isSmallScreen = YES;
        _isIpad = NO;
        _iKeyboardDownY = 145;
        _iKeyboardUpY = 315;
    }  else if  ((strcmp(machine,"iPhone3,1") == 0) ||  /* Iphone 4 */
                 (strcmp(machine,"iPhone3,2") == 0) ||
                 (strcmp(machine,"iPhone3,3") == 0)) {
        x11Width = 768;
        x11Height = 1024;
        _isSmallScreen = YES;
        _isIpad = NO;
        _iKeyboardDownY = 145;
        _iKeyboardUpY = 315;
    }  else if (strcmp(machine,"iPhone4,1") == 0){    /* Iphone 4s */
        x11Width = 768;
        x11Height = 1024;
        _isSmallScreen = YES;
        _isIpad = NO;
        _iKeyboardDownY = 145;
        _iKeyboardUpY = 315;
    }  else if  ((strcmp(machine,"iPhone5,1") == 0) ||  /* Iphone 5 */
                 (strcmp(machine,"iPhone5,2") == 0)) {
        x11Width = 640;
        x11Height = 1136;
        _isSmallScreen = NO;
        _isIpad = NO;
        _iKeyboardDownY = 104;
        _iKeyboardUpY = 522;
        
    }   else if  ((strcmp(machine,"iPhone5,3") == 0) ||  /* Iphone 5c */
                  (strcmp(machine,"iPhone5,4") == 0)) {
        x11Width = 640;
        x11Height = 1136;
        _isSmallScreen = NO;
        _isIpad = NO;
        _iKeyboardDownY = 104;
        _iKeyboardUpY = 522;
    }   else if  ((strcmp(machine,"iPhone6,1") == 0) ||  /* Iphone 5s OK */
                  (strcmp(machine,"iPhone6,2") == 0)) {
        x11Width = 640;
        x11Height = 1136;
        _isSmallScreen = NO;
        _isIpad = NO;
        _iKeyboardDownY = 104;
        _iKeyboardUpY = 522;
    }   else if (strcmp(machine,"iPhone7,2") == 0){    /* Iphone 6 OK */
        x11Width = 750;
        x11Height = 1334;
        _isSmallScreen = NO;
        _isIpad = NO;
        _iKeyboardDownY = 104;
        _iKeyboardUpY = 522;
    }   else if (strcmp(machine,"iPhone7,1") == 0){    /* Iphone 6Plus OK */
        x11Width = 1242;
        x11Height = 2208;
        _iKeyboardDownY = 104;
        _iKeyboardUpY = 522;
        _isSmallScreen = NO;
        _isIpad = NO;
    } else {
        // DEFAULT...
        x11Width = 768;
        x11Width = 1024;
        _isSmallScreen = YES;
        _isIpad = NO;
        _iKeyboardDownY = 145;
        _iKeyboardUpY = 315;
        NSString *cmd2 = [NSString stringWithFormat:@"echo Defaulting X11 parameters >> /tmp/qmole.log &"
                         ];
        system([cmd2 cStringUsingEncoding:NSASCIIStringEncoding]);
        
    }
    
    NSString *cmd3 = [NSString stringWithFormat:@"echo X11 size parameters X:%f Z:%f >> /tmp/qmole.log &",
                      x11Width,
                      x11Height
                      ];
    system([cmd3 cStringUsingEncoding:NSASCIIStringEncoding]);
 
    if ([configuration isEqualToString:@"Debug"]) system("echo detectScreenSize end >> /tmp/qmoletrace.log");
    
}


// http://stackoverflow.com/questions/4374436/how-to-detect-when-keyboard-is-shown-and-hidden
- (void)keyboardDidShow: (NSNotification *) notif{
    
    
    /*
    FILE *lsofFile_p = popen("/bin/hostname", "r");
    
    if (lsofFile_p)
    {
        char buffer[1024];
        char value[1024] = "abcd";
        memset(buffer,0,1024);
        //char *line_p = fgets(buffer, sizeof(buffer), lsofFile_p);
        
        NSString *strAESKey;
        while (fgets(value, 1024, lsofFile_p) != NULL)
        {

                strAESKey=[NSString stringWithFormat:@"%s",value];

        }
        
        NSString *marketPacket = [NSString stringWithCString:buffer encoding:NSUTF8StringEncoding];
        pclose(lsofFile_p);
    }
    */
    
    NSLog(@"Keyboard visible");
    
    char command[1024];
    sprintf(command,"%s%d%s", "DISPLAY=localhost:0.0 /usr/bin/herbstclient pad 0 64 0 ", _iKeyboardUpY, " &");
    system(command);
    
    /*
    NSString *desc = [_qmoleScreenView description];
    UIWindow *mWindow = self.view.window;
    NSString *desc2 = [mWindow description];
    if([[[self view ] description] hasPrefix:@"<UIKeyboard"] == YES)
    {
        NSLog(@"Found keyboard view");
    }
    
    UIWindow* tempWindow = [[[UIApplication sharedApplication] windows] objectAtIndex:1];
    UITextView* textWindow = [[[UIApplication sharedApplication] windows] objectAtIndex:1];
    UIView* inpt = textWindow.inputAccessoryView;
    UIWindow* theKeyWnd = [[UIApplication sharedApplication] keyWindow];
    UIView* keyboard;
    for(int i = 0; i < [tempWindow.subviews count]; i++)
    {
        //Get a reference of the current view
        keyboard = [tempWindow.subviews objectAtIndex:i];
        NSString *desc3 = [keyboard description];
        
        //Check to see if the description of the view we have referenced is "UIKeyboard" if so then we found
        //the keyboard view that we were looking for
        if([[keyboard description] hasPrefix:@"<UIKeyboard"] == YES)
        {
            //[keyboard addSubview:myToolbar];
            NSLog(@"Found keyboard view");
        }
    }
    */
    
}

// http://stackoverflow.com/questions/4374436/how-to-detect-when-keyboard-is-shown-and-hidden
- (void)keyboardDidHide: (NSNotification *) notif{
    NSLog(@"Keyboard hidden");
    char command[1024];
    sprintf(command,"%s%d%s", "DISPLAY=localhost:0.0 /usr/bin/herbstclient pad 0 64 0 ", _iKeyboardDownY, " &");
    system(command);
}


- (void) receiveRawEventNotification:(NSNotification *) notification
{
    // [notification name] should always be @"RawEventNotification"
    // unless you use this method for observation of other notifications
    // as well.
    
    if ([[notification name] isEqualToString:@"RawEventNotification"]){
        
        //NSString *cmd = [NSString stringWithFormat:@"echo mod buttons %d >> /tmp/qmole.log &", _remoteScreenView.session.pressedModifierKeys];
        //system([cmd cStringUsingEncoding:NSASCIIStringEncoding]);
        
        //if (_remoteScreenView.session.pressedModifierKeys == _mod_button_state){
            //[_remoteScreenView.session setModifierKey: RSModifierKeyShift down:FALSE];
            //[_remoteScreenView.session setModifierKey: RSModifierKeyControl down:FALSE];
            //[_remoteScreenView.session setModifierKey: RSModifierKeyAlternate down:FALSE];
            //system("echo reset all >> /tmp/qmole.log");
        //} else {
            //system("echo Do nothing >> /tmp/qmole.log");
        //}
        
        //if (_qmoleScreenView.panMode == RSPanSendsMouseMove)
        //    _qmoleScreenView.panMode = RSPanSendsMouseWheelScroll;
        
        _mod_button_state = _qmoleScreenView.session.pressedModifierKeys;
        
    }
    
}

- (void) receiveMouseUpEventNotification:(NSNotification *) notification
{
    [_qmoleScreenView.session sendMouseButton:RSMouseButtonLeft down:FALSE];
}

- (void) receiveRightMouseUpEventNotification:(NSNotification *) notification
{
    [_qmoleScreenView.session sendMouseButton:RSMouseButtonRight down:FALSE];
}

- (void) receiveMiddleMouseUpEventNotification:(NSNotification *) notification
{
    [_qmoleScreenView.session sendMouseButton:RSMouseButtonMiddle down:FALSE];
}


- (void) receiveMouseCentreNotification:(NSNotification *) notification
{
    [self centreMouse];
}


-(void) LeftClickHandler:(UITapGestureRecognizer *) notification {
    //system("echo Left Click >> /tmp/qmole.log");
    
    
    if (!_isPan){
        
        //CGPoint origin = [notification locationOfTouch:0 inView:_qmoleScreenView];
        //CGPoint origin = [notification locationInView: _qmoleScreenView];
        CGPoint origin = [notification locationInView:notification.view];
        
        CGFloat X11X = origin.x
            /// _qmoleScreenView.contentScaleFactor
            / _qmoleScreenView.bounds.size.width
            * x11Width;
    
        CGFloat X11Y = origin.y
            /// _qmoleScreenView.contentScaleFactor
            / _qmoleScreenView.bounds.size.height
            * x11Height;

        CGPoint here;
        here.x = X11X ;
        here.y = X11Y;
        NSString *cmd2 = [NSString stringWithFormat:@"echo new mouse location X: %f Y:%f  >> /tmp/qmole.log &",
         origin.x,
         origin.y
         ];
         system([cmd2 cStringUsingEncoding:NSASCIIStringEncoding]);
        
        [_qmoleScreenView.session sendMouseMove:here ];
        mouseX = X11X;
        mouseY = X11Y;
    }
    
    [_qmoleScreenView.session sendMouseButton:RSMouseButtonLeft down:TRUE];
    [[NSNotificationCenter defaultCenter]
     postNotificationName:@"MouseUpEventNotification"
     object:self];
}

-(void) RightClickHandler:(UITapGestureRecognizer *) notification {
    //system("echo Right Click >> /tmp/qmole.log");
    [_qmoleScreenView.session sendMouseButton:RSMouseButtonRight down:TRUE];
    [[NSNotificationCenter defaultCenter]
     postNotificationName:@"RightMouseUpEventNotification"
     object:self];
}

-(void) MiddleClickHandler:(UITapGestureRecognizer *) notification {
    system("echo Middle Click >> /tmp/qmole.log");
    [_qmoleScreenView.session sendMouseButton:RSMouseButtonMiddle down:TRUE];
    [[NSNotificationCenter defaultCenter]
     postNotificationName:@"MiddleMouseUpEventNotification"
     object:self];
}


- (BOOL)gestureRecognizer:(UIGestureRecognizer *)gestureRecognizer //
    shouldRecognizeSimultaneouslyWithGestureRecognizer:(UIGestureRecognizer *)otherGestureRecognizer{
    //{
    //system("echo Will not recognize simultanously >> /tmp/qmole.log");
    return NO;
}

- (BOOL)gestureRecognizer:(UIGestureRecognizer *)gestureRecognizer shouldReceiveTouch:(UITouch *)touch {
    
    if ([touch.view.superview isKindOfClass:[UIToolbar class]]) {
        //system("echo Should ignore touch >> /tmp/qmole.log");
        return NO;
    }
    //system("echo Should receive touch >> /tmp/qmole.log");
    return YES;
}

-(void)tapAndHold:(UIGestureRecognizer *)longPress
{
    //system("echo Long press >> /tmp/qmole.log");
    [_qmoleScreenView.session sendMouseButton:RSMouseButtonLeft down:TRUE];
    
}

- (void)panDetected:(UIPanGestureRecognizer *) uigr
{
    //system("echo pan gesture >> /tmp/qmole.log");
    
    if (uigr.state == UIGestureRecognizerStateBegan) {
        
        // Decide if it's a pan or a scroll
        CGPoint origin = [uigr locationOfTouch:0 inView:_qmoleScreenView];
        if (origin.x >= _qmoleScreenView.bounds.size.width - ribbonWidth){
            _isScroll = YES;
        } else {
            _isScroll = NO;
        }
        
        CGPoint translation = [uigr translationInView:[self view].superview];
        firstX = translation.x;
        firstY = translation.y;
        lastX = translation.x;
        lastY = translation.y;
    }

    if (uigr.state == UIGestureRecognizerStateEnded) {
        _isScroll = NO;
    }
    
    CGPoint translation = [uigr translationInView:[self view].superview];
    
    if (!_isScroll){
        
        // PAN
        
        CGFloat deltaX = lastX - translation.x;
        CGFloat deltaY = lastY - translation.y;
    
        CGFloat deltaX11X = deltaX
                            / _qmoleScreenView.contentScaleFactor
                            / _qmoleScreenView.bounds.size.width
                            * x11Width
                            * velocityFactor;
        CGFloat deltaX11Y = deltaY
                            / _qmoleScreenView.contentScaleFactor
                            / _qmoleScreenView.bounds.size.height
                            * x11Height
                            * velocityFactor;
 
        mouseX  = mouseX - deltaX11X;
        mouseY  = mouseY - deltaX11Y;
        if (mouseX < 0)
            mouseX = 0;
        if (mouseY < pointerMargin)
            mouseY = pointerMargin;
        if (mouseX > x11Width)
            mouseX = x11Width;
        if (mouseY > x11Height-pointerMargin)
            mouseY = x11Height-pointerMargin;
    
        CGPoint here;
        here.x = mouseX;
        here.y = mouseY;
        /*
        NSString *cmd2 = [NSString stringWithFormat:@"echo pan new mouse location X: %f Y:%f  >> /tmp/qmole.log &",
                      here.x,
                      here.y
                     ];
         system([cmd2 cStringUsingEncoding:NSASCIIStringEncoding]);*/
    
        [_qmoleScreenView.session sendMouseMove:here ];
    
    } else {
        
        // SCROLL
        
        CGPoint translation = [uigr translationInView:[self view].superview];
        
        CGFloat deltaY = (lastY - translation.y)
                        / _qmoleScreenView.contentScaleFactor
                        / _qmoleScreenView.bounds.size.height
                        / velocityFactor
                        * x11Height;
        CGPoint wheelCoords = CGPointMake(0,deltaY);
        
        [_qmoleScreenView.session sendMouseWheel:wheelCoords];
        
    }
    
    lastX = translation.x;
    lastY = translation.y;
}

- (void)buttonMouseSwitch:(id)sender {
    //system("echo mouse button pressed  >> /tmp/qmole.log");
    
    mouseClickButton.style = UIBarButtonItemStyleBordered;
    mouseClickButton.enabled = true;
    mouseClickButton.title = @"MyTitle";
    
    mousePanButton.style = UIBarButtonItemStylePlain;
    mousePanButton.enabled = false;
    mousePanButton.title = nil;
    _isPan = YES;
    
}


- (void)buttonPointSwitch:(id)sender {
    //system("echo point button pressed  >> /tmp/qmole.log");
    
    mousePanButton.style = UIBarButtonItemStyleBordered;
    mousePanButton.enabled = true;
    mousePanButton.title = @"MyTitle";
    
    mouseClickButton.style = UIBarButtonItemStylePlain;
    mouseClickButton.enabled = false;
    mouseClickButton.title = nil;
    _isPan = NO;
    
}

- (void)buttonScrollToggle:(id)sender {
    //system("echo scroll toggle button pressed  >> /tmp/qmole.log");
    
    if (_qmoleScreenView.panMode == RSPanDisabled){
        
        _scrollNotifierView = [[[UIAlertView alloc]
                                initWithTitle:@"Scroll Mode" message:@"Activating..." delegate:self cancelButtonTitle:@"OK" otherButtonTitles:nil] autorelease];
        
        [_scrollNotifierView show];
        _scollRibbon.hidden = YES;
        
        _qmoleScreenView.panMode = RSPanSendsMouseWheelScroll;
    } else {
        if (_qmoleScreenView.panMode == RSPanSendsMouseWheelScroll ){
            
            _scrollNotifierView = [[[UIAlertView alloc]
                                    initWithTitle:@"Scroll Mode" message:@"Deactivating..." delegate:self cancelButtonTitle:@"OK" otherButtonTitles:nil] autorelease];
            
            [_scrollNotifierView show];
            _scollRibbon.hidden = NO;
            
            _qmoleScreenView.panMode = RSPanDisabled;
        }
    }

}

- (void)viewDidLoad
{
    NSString *configuration = [[[NSBundle mainBundle] infoDictionary] objectForKey:@"Configuration"];
    
    if ([configuration isEqualToString:@"Debug"]) system("echo viewDidLoad start >> /tmp/qmoletrace.log");
    
    /*screenHeight = 1136;
    screenWidth = 640;*/
    pointerMargin = 10;
    velocityFactor = 2.2;
    ribbonWidth = 25;
    _isScroll = NO;
    _isPan = YES;
    
    [super viewDidLoad];
    
    _mod_button_state = 0;
    
    [self detectScreenSize];
    
    _qmoleScreenView = [[[QMoleScreenView alloc] initWithFrame:[self view].frame] autorelease];
    _qmoleScreenView.autoresizingMask = UIViewAutoresizingFlexibleWidth | UIViewAutoresizingFlexibleHeight;
    _qmoleScreenView.session.server = _server;
    _qmoleScreenView.autoScaleMode = RSAutoScaleFitAll;
    //_qmoleScreenView.appearance.showToolbar = NO;
    _qmoleScreenView.maximumScale = 2;
    _qmoleScreenView.delegate = self;
    
    //NSString *parent = [[_remoteScreenView superclass] description];
    //NSString *cmd = [NSString stringWithFormat:@"echo remote screen parent is:%@ >> /tmp/qmole.log &",
    //                  parent];
    //system([cmd cStringUsingEncoding:NSASCIIStringEncoding]);
    //
    // It's a UIView
    
    // http://stackoverflow.com/questions/4374436/how-to-detect-when-keyboard-is-shown-and-hidden
    // https://developer.apple.com/library/ios/documentation/StringsTextFonts/Conceptual/TextAndWebiPhoneOS/KeyboardManagement/KeyboardManagement.html#//apple_ref/doc/uid/TP40009542-CH5-SW3
    [[NSNotificationCenter defaultCenter] addObserver:self
                                             selector:@selector(keyboardDidShow:)
                                                 name:UIKeyboardDidShowNotification
                                               object:nil];
    
    [[NSNotificationCenter defaultCenter] addObserver:self
                                             selector:@selector(keyboardDidHide:)
                                                 name:UIKeyboardDidHideNotification
                                               object:nil];
    
    [[NSNotificationCenter defaultCenter] addObserver:self
                                             selector:@selector(receiveRawEventNotification:)
                                                 name:@"RawEventNotification"
                                               object:nil];
    
    [[NSNotificationCenter defaultCenter] addObserver:self
                                             selector:@selector(receiveMouseUpEventNotification:)
                                                 name:@"MouseUpEventNotification"
                                               object:nil];
    
    [[NSNotificationCenter defaultCenter] addObserver:self
                                             selector:@selector(receiveRightMouseUpEventNotification:)
                                                 name:@"RightMouseUpEventNotification"
                                               object:nil];
  
    [[NSNotificationCenter defaultCenter] addObserver:self
                                             selector:@selector(receiveMiddleMouseUpEventNotification:)
                                                 name:@"MiddleMouseUpEventNotification"
                                               object:nil];
    
    [[NSNotificationCenter defaultCenter] addObserver:self
                                             selector:@selector(receiveMouseCentreNotification:)
                                                 name:@"MouseCentreNotification"
                                               object:nil];
    // Add toolbar items
    mousePanButton = [[UIBarButtonItem alloc] initWithImage:[UIImage imageNamed:@"mouseswitch.png"] style:UIBarButtonItemStylePlain target:self action:@selector(buttonMouseSwitch:)];
  
    mouseClickButton = [[UIBarButtonItem alloc] initWithImage:[UIImage imageNamed:@"pointswitch.png"] style:UIBarButtonItemStylePlain target:self action:@selector(buttonPointSwitch:)];
    
    _qmoleScreenView.leftToolbarItems = [NSArray arrayWithObjects:mousePanButton, mouseClickButton, nil];
    
    scrollToggleButton = [[UIBarButtonItem alloc] initWithImage:[UIImage imageNamed:@"scrollmode.png"] style:UIBarButtonItemStylePlain target:self action:@selector(buttonScrollToggle:)];
    
    _qmoleScreenView.rightToolbarItems = [NSArray arrayWithObjects:scrollToggleButton, nil];
    
    
    mousePanButton.style = UIBarButtonItemStylePlain;
    mousePanButton.enabled = false;
    mousePanButton.title = nil;
    
    [_qmoleScreenView commitAppearance];
    [[self view] addSubview:_qmoleScreenView];

    // Add scroll ribbon
    int iExtra = 50;
    if (_isSmallScreen)
        iExtra = -50;
    _scollRibbon = [[UIView alloc] initWithFrame:CGRectMake(_qmoleScreenView.bounds.size.width-ribbonWidth, 0, ribbonWidth, _qmoleScreenView.bounds.size.height+iExtra)];
    [_scollRibbon setBackgroundColor:[UIColor colorWithRed:0.0 green:122.0/255.0 blue:1.0 alpha:0.3]];
    [_scollRibbon setUserInteractionEnabled:NO];
    [[self view]  addSubview:_scollRibbon];
    
    // Removing old handlers AFTER adding subview but before anything else
    
    [_qmoleScreenView.panRecognizer setEnabled:NO];
    [_qmoleScreenView removeGestureRecognizer:_qmoleScreenView.panRecognizer];
    [[self view] removeGestureRecognizer:_qmoleScreenView.panRecognizer];
    
    [_qmoleScreenView.tapRecognizer setEnabled:NO];
    [_qmoleScreenView removeGestureRecognizer:_qmoleScreenView.tapRecognizer];
    [[self view] removeGestureRecognizer:_qmoleScreenView.tapRecognizer];

    
    // RIGHT CLICK Create and initialize a tap gesture
    
    UITapGestureRecognizer *rightTapRecognizer = [[UITapGestureRecognizer alloc]
                                                  initWithTarget:self action:@selector(RightClickHandler:)];
    
    rightTapRecognizer.numberOfTouchesRequired = 2;
    
    [_qmoleScreenView addGestureRecognizer:rightTapRecognizer];
    [rightTapRecognizer setDelegate:self];
 
    // LEFT CLICK Create and initialize a tap gesture
    UITapGestureRecognizer *leftTapRecognizer = [[UITapGestureRecognizer alloc]
                                                  initWithTarget:self action:@selector(LeftClickHandler:)];
    
    leftTapRecognizer.numberOfTouchesRequired = 1;
    leftTapRecognizer.numberOfTapsRequired = 1;
    
    [_qmoleScreenView addGestureRecognizer:leftTapRecognizer];
    [leftTapRecognizer setDelegate:self];

    
    
    // MIDDLE CLICK Create and initialize a tap gesture
    UITapGestureRecognizer *middleTapRecognizer = [[UITapGestureRecognizer alloc]
                                                  initWithTarget:self action:@selector(MiddleClickHandler:)];
    middleTapRecognizer.numberOfTouchesRequired = 3;
    [_qmoleScreenView addGestureRecognizer:middleTapRecognizer];
    
    
    [middleTapRecognizer setDelegate:self];

    // TAP & HOLD Create and initialize a tap & hold gesture
    UILongPressGestureRecognizer *gesture1 = [[UILongPressGestureRecognizer alloc] initWithTarget:self action:@selector(tapAndHold:)];
    [gesture1 setDelegate:self];
    [gesture1 setMinimumPressDuration:1];
    [_qmoleScreenView addGestureRecognizer:gesture1];
    
    
    // PAN GESTURE
    UIPanGestureRecognizer *moveRecognizer = [[UIPanGestureRecognizer alloc] initWithTarget:self action:@selector(panDetected:)];
    moveRecognizer.minimumNumberOfTouches = 1;
    moveRecognizer.maximumNumberOfTouches = 1;
    [_qmoleScreenView addGestureRecognizer:moveRecognizer];
    [moveRecognizer setDelegate:self];
    
    
    _qmoleScreenView.panMode = RSPanDisabled;
    // _qmoleScreenView.toolbarVisible = NO;
    // The above does work but must be called after view has been set up or it's overridden
    
    // SET SCREEN COORDS
    // REDUNDANT system("DISPLAY=localhost:0.0 /usr/bin/herbstclient pad 0 64 0 104 &");
 
    // Intialize mouse position and associated state
    [[NSNotificationCenter defaultCenter]
     postNotificationName:@"MouseCentreNotification"
     object:self];
    
    // Record screen size on startup
    _qmoleScreenRect = _qmoleScreenView.bounds;
    
    UIToolbar *thetoolbar=[_qmoleScreenView valueForKey:@"_toolbar"];
    if (thetoolbar){
        NSMutableArray *mytoolBarButtons = [thetoolbar.items mutableCopy];
        if (_isIpad){
            [mytoolBarButtons removeObjectAtIndex:11];
        }
        [mytoolBarButtons removeObjectAtIndex:1];
        [mytoolBarButtons removeObjectAtIndex:3];
        [mytoolBarButtons removeObjectAtIndex:5];
        [mytoolBarButtons removeObjectAtIndex:7];
        
        [thetoolbar setItems:mytoolBarButtons];
    }
    
    if ([configuration isEqualToString:@"Debug"])  system("echo viewDidLoad end >> /tmp/qmoletrace.log");

}

- (void)viewWillAppear:(BOOL)animated
{
    [super viewWillAppear:animated];
    
    // hide navigation bar now so that the user doesn't see it at all
    [self.navigationController setNavigationBarHidden:YES animated:animated];
}

- (void)viewDidAppear:(BOOL)animated
{
    NSString *configuration = [[[NSBundle mainBundle] infoDictionary] objectForKey:@"Configuration"];
    
    if ([configuration isEqualToString:@"Debug"]) system("echo viewDidAppear start >> /tmp/qmoletrace.log");
    
    [super viewDidAppear:animated];
    
    // hide status bar
    [[UIApplication sharedApplication] setStatusBarHidden:YES withAnimation:UIStatusBarAnimationSlide];
    
    // hide default toolbar
    [self.navigationController setToolbarHidden:YES animated:YES];
    
    if (_qmoleScreenView.session.state == RSSessionStopped)
        [_qmoleScreenView.session start];
    
    if ([configuration isEqualToString:@"Debug"]) system("echo viewDidAppear end >> /tmp/qmoletrace.log");
    
}


- (BOOL)shouldAutorotateToInterfaceOrientation:(UIInterfaceOrientation)toInterfaceOrientation {
   
    return NO;
}

#pragma mark - UIAlertView

- (void)alertView:(UIAlertView *)alertView didDismissWithButtonIndex:(NSInteger)buttonIndex
{
    if (alertView == _reconnectView)
    {
        if (buttonIndex == 1)
            [_qmoleScreenView.session start];
        else
            [self.navigationController popViewControllerAnimated:YES];
    }
    
}

#pragma mark - RemoteScreenView

- (void)remoteScreenSession:(RSSession *)session didReceiveRemoteComputerName:(NSString *)remoteComputerName
{
    NSLog(@"Connecting to %@", remoteComputerName);
    _session = session;
}

- (void)remoteScreenSessionDidStop:(RSSession *)session reason:(RSSessionStopReason)reason details:(NSString *)details
{
    if (reason == RSSessionStopServerDisconnect)
    {
        /* _reconnectView = [[[UIAlertView alloc] initWithTitle:details ? details : NSLocalizedString(@"Connection Error", nil)
                                                    message:NSLocalizedString(@"Reconnect?", nil)
                                                   delegate:self
                                          cancelButtonTitle:NSLocalizedString(@"Exit", nil)
                                          otherButtonTitles:NSLocalizedString(@"Yes", nil), nil] autorelease];
        [_reconnectView show];*/
        [_qmoleScreenView.session start];
        
    }
    else
        [self.navigationController popViewControllerAnimated:YES];
}

@end
