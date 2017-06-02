//
//  AppDelegate.h
//  QMole
//
//  Created by Maksym Huk on 8/29/12.
//  Copyright (c) 2012 Abto Software. All rights reserved.
//

#import <UIKit/UIKit.h>

@class AuthViewController;

@interface AppDelegate : UIResponder <UIApplicationDelegate>
{
 UIBackgroundTaskIdentifier bgTask;
 BOOL inBackground;
}
@property (nonatomic, retain) UIWindow *window;


@end
