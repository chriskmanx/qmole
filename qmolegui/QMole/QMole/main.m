//
//  main.m
//  QMole
//
//  Created by Maksym Huk on 8/29/12.
//  Copyright (c) 2012 Abto Software. All rights reserved.
//

#import <UIKit/UIKit.h>

#import "MyApplication.h"
#import "AppDelegate.h"

int main(int argc, char *argv[])
{
    NSString *configuration = [[[NSBundle mainBundle] infoDictionary] objectForKey:@"Configuration"];
    
    if ([configuration isEqualToString:@"Debug"]) system("echo QMole::main start > /tmp/qmoletrace.log");
    
    NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
    
  
    
    int result = UIApplicationMain(argc,
                                   argv,
                                   NSStringFromClass([MyApplication class]),
                                   NSStringFromClass([AppDelegate class]));
    [pool drain];
    
    if ([configuration isEqualToString:@"Debug"]) system("echo QMole::main end >> /tmp/qmoletrace.log");
    return result;
}
