//
//  main.m
//  qmoledevice
//
//  Created by Chris Kohlhepp on 2/05/2015.
//  Copyright (c) 2015 Chris Kohlhepp. All rights reserved.
//
#import <Foundation/NSString.h>
#import <sys/sysctl.h>

int main(int argc, char * argv[])
{

    size_t size;
    sysctlbyname("hw.machine", NULL, &size, NULL, 0);
    char *machine = malloc(size);
    sysctlbyname("hw.machine", machine, &size, NULL, 0);
    NSString *platform = [NSString stringWithCString:machine encoding:NSUTF8StringEncoding];
    NSLog(@"Device %@",platform);
    
    //NSString *udid = [[[UIDevice currentDevice]identifierForVendor]UUIDString];
    //UIDevice *myDevice = [UIDevice currentDevice];
    //NSString *deviceOSVersion = myDevice.systemVersion;
    //NSLog(@"OS Version %@",deviceOSVersion);
    
    return 0;

}
