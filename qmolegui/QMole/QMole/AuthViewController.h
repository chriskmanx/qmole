//
//  ViewController.h
//  QMole
//
//  Created by Maksym Huk on 8/29/12.
//  Copyright (c) 2012 Abto Software. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "RSServer.h"


/* UITextFieldDelegate, */

@interface AuthViewController : UITableViewController <UITextViewDelegate>
{
    NSArray *_rowNames;
    RSServer *_server;
    BOOL _licenseAccepted;
    UITextView *newTextView;
    NSMutableArray *_buttons;
    UIButton *_accept;
    UIButton *_donate;
    UIButton *_contact;
    CGFloat _screenWidth;
    CGFloat _screenHeight;
    UITextView *_myTextView;
}
@end

