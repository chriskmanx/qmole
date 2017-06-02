//
//  MyApplication.m
//  QMole
//
//  Created by Chris Kohlhepp on 30/07/2015.
//  Copyright (c) 2015 QMole Ltd. All rights reserved.
//

#import <UIKit/UIEvent.h>
#import "MyApplication.h"

@implementation MyApplication

#define GSEVENT_TYPE 2
#define GSEVENT_FLAGS 12
#define GSEVENTKEY_KEYCODE 15
#define GSEVENT_TYPE_KEYUP 11


- (void)sendEvent:(UIEvent *)event
{
    [super sendEvent:event];
    
    [[NSNotificationCenter defaultCenter]
     postNotificationName:@"RawEventNotification"
     object:self];
}



@end


