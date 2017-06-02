//
//  ViewController.h
//  Vnc
//
//  Created by Maksym Huk on 7/31/12.
//  Copyright (c) 2012 Abto Software. All rights reserved.
//

#import <UIKit/UIKit.h>

#import "QMoleScreenView.h"

@interface RemoteScreenViewController : UIViewController <RSSessionDelegate, UIGestureRecognizerDelegate>
{
    QMoleScreenView *_qmoleScreenView;
    UIAlertView *_reconnectView;
    UIAlertView *_scrollNotifierView;
    UIView *_scollRibbon;              // scroll ribbon oberlay
                                       // works because tiling window
                                       // manager guarantees that
                                       // all windows are right flush
    RSSession *_session;
    
    BOOL _isScroll;                    // is pan a scroll
    int _mod_button_state;
    
    UIBarButtonItem *mousePanButton;
    UIBarButtonItem *mouseClickButton;
    UIBarButtonItem *scrollToggleButton;
    
    CGRect _qmoleScreenRect;
    
    BOOL _isPan;
    
    CGFloat firstX;
    CGFloat firstY;
    CGFloat lastX;
    CGFloat lastY;
    
    int _iKeyboardUpY;
    int _iKeyboardDownY;
    
    CGFloat ribbonWidth;
    
    CGFloat mouseX;
    CGFloat mouseY;
    BOOL _isSmallScreen;
    BOOL _isIpad;
  
    /*CGFloat screenHeight;
    CGFloat screenWidth;*/

    CGFloat x11Height;
    CGFloat x11Width;
    
    CGFloat velocityFactor;
    
    CGFloat pointerMargin;
}

@property (nonatomic, retain) RSServer *server;

- (void)centreMouse;
- (void)detectScreenSize;
- (NSString *)pathForTemporaryFileWithPrefix:(NSString *)prefix;

@end