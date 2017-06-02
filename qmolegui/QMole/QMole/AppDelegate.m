//
//  AppDelegate.m
//  QMole
//
//

#import "AppDelegate.h"

#import "AuthViewController.h"

#define SYSTEM_VERSION_LESS_THAN(v)                 ([[[UIDevice currentDevice] systemVersion] compare:v options:NSNumericSearch] == NSOrderedAscending)


@implementation AppDelegate

@synthesize window=_window;

- (void)dealloc
{
    [_window release];
    [super dealloc];
}

- (void)timerProc:(NSTimer*)timer {
    NSString *configuration = [[[NSBundle mainBundle] infoDictionary] objectForKey:@"Configuration"];
  
    if ([configuration isEqualToString:@"Debug"]) system("echo timerProc start >> /tmp/qmoletrace.log");
    
    [[UIApplication sharedApplication] endBackgroundTask:bgTask];
    
    
    bgTask = [[UIApplication sharedApplication]
              beginBackgroundTaskWithExpirationHandler:^{
                  //   If you're worried about exceeding 3 minutes, handle it here
                  system("echo QMole background timer expired >> /tmp/qmolebackground.log");
              }];
    system("echo QMole background timer task>> /tmp/qmolebackground.log");
    
    if ([configuration isEqualToString:@"Debug"]) system("echo timerProc end >> /tmp/qmoletrace.log");
}

- (BOOL)application:(UIApplication *)application didFinishLaunchingWithOptions:(NSDictionary *)launchOptions
{
    NSString *configuration = [[[NSBundle mainBundle] infoDictionary] objectForKey:@"Configuration"];
    
    if ([configuration isEqualToString:@"Debug"]) system("echo UIApplication::didFinishLaunchingWithOptions start >> /tmp/qmoletrace.log");
    
    NSString *nibName = [[UIDevice currentDevice] userInterfaceIdiom] == UIUserInterfaceIdiomPhone ? @"AuthViewController_iPad" : @"AuthViewController_iPad";
    AuthViewController *viewController = [[[AuthViewController alloc] initWithNibName:nibName bundle:nil] autorelease];
    
    UINavigationController *navigationController = [[[UINavigationController alloc] initWithRootViewController:viewController] autorelease];
    
    self.window = [[[UIWindow alloc] initWithFrame:[[UIScreen mainScreen] bounds]] autorelease];
    self.window.rootViewController = navigationController;
    [self.window makeKeyAndVisible];
    
    system("echo QMole check backgrounding... > /tmp/qmolebackground.log");
    
    inBackground = NO;
    
    if (SYSTEM_VERSION_LESS_THAN(@"7.0.2")) {
        inBackground = NO;
        if ([configuration isEqualToString:@"Debug"]) system("echo UIApplication::didFinishLaunchingWithOptions skip backgrounding >> /tmp/qmoletrace.log");
    } else {
        if ([configuration isEqualToString:@"Debug"])
            system("echo UIApplication::didFinishLaunchingWithOptions initiate backgrounding >> /tmp/qmoletrace.log");

        bgTask = [[UIApplication sharedApplication]
                  beginBackgroundTaskWithExpirationHandler:^{
                  //   If you're worried about exceeding 10 minutes, handle it here
                  }];
    
        [NSTimer scheduledTimerWithTimeInterval:2.0f*60.0f  // 2 minutes
                                         target:self
                                         selector:@selector(timerProc:)
                                         userInfo:nil
                                         repeats:YES];
    }
    

    if ([configuration isEqualToString:@"Debug"]) system("echo UIApplication::didFinishLaunchingWithOptions end >> /tmp/qmoletrace.log");
    
    return YES;
}

- (void)applicationWillResignActive:(UIApplication *)application
{
    // Sent when the application is about to move from active to inactive state. This can occur for certain types of temporary interruptions (such as an incoming phone call or SMS message) or when the user quits the application and it begins the transition to the background state.
    // Use this method to pause ongoing tasks, disable timers, and throttle down OpenGL ES frame rates. Games should use this method to pause the game.
}


- (void)applicationWillEnterForeground:(UIApplication *)application
{
    NSString *configuration = [[[NSBundle mainBundle] infoDictionary] objectForKey:@"Configuration"];
    
    if ([configuration isEqualToString:@"Debug"]) system("echo UIApplication::applicationWillEnterForeground start >> /tmp/qmoletrace.log");
    
    inBackground = NO;
    system("echo QMole going to foreground >> /tmp/qmolebackground.log");
 
    if ([configuration isEqualToString:@"Debug"]) system("echo UIApplication::applicationWillEnterForeground end >> /tmp/qmoletrace.log");
}

- (void)applicationDidBecomeActive:(UIApplication *)application
{
    NSString *configuration = [[[NSBundle mainBundle] infoDictionary] objectForKey:@"Configuration"];
    
    if ([configuration isEqualToString:@"Debug"]) system("echo UIApplication::applicationDidBecomeActive start >> /tmp/qmoletrace.log");
    
    // Restart any tasks that were paused (or not yet started) while the application was inactive. If the application was previously in the background, optionally refresh the user interface.
    if ([configuration isEqualToString:@"Debug"]) system("echo UIApplication::applicationWillEnterForeground end >> /tmp/qmoletrace.log");
    
    if ([configuration isEqualToString:@"Debug"]) system("echo UIApplication::applicationDidBecomeActive end >> /tmp/qmoletrace.log");
    
}

- (void)applicationWillTerminate:(UIApplication *)application
{
    NSString *configuration = [[[NSBundle mainBundle] infoDictionary] objectForKey:@"Configuration"];
    
    if ([configuration isEqualToString:@"Debug"]) system("echo UIApplication::applicationWillTerminate start >> /tmp/qmoletrace.log");
    
    // Called when the application is about to terminate. Save data if appropriate. See also applicationDidEnterBackground:.
    system("echo QMole terminating >> /tmp/qmolebackground.log");
    
    if ([configuration isEqualToString:@"Debug"]) system("echo UIApplication::applicationWillTerminate end >> /tmp/qmoletrace.log");
}



- (void)applicationDidEnterBackground:(UIApplication *)application {
    NSString *configuration = [[[NSBundle mainBundle] infoDictionary] objectForKey:@"Configuration"];
    
    if ([configuration isEqualToString:@"Debug"]) system("echo UIApplication::applicationDidEnterBackground start >> /tmp/qmoletrace.log");
    
    inBackground = YES;
    system("echo QMole going to background >> /tmp/qmolebackground.log");
    
    if ([configuration isEqualToString:@"Debug"]) system("echo UIApplication::applicationDidEnterBackground end >> /tmp/qmoletrace.log");
}



@end
