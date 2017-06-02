//
//  AuthViewController.m
//  QMole
//
//  Created by Christoph Kohlhepp
//  Copyright (c) 2015 Christoph Kohlhepp. All rights reserved.
//

#import "AuthViewController.h"
#import "RemoteScreenViewController.h"
#import <CommonCrypto/CommonDigest.h>
#import <mach/mach.h>
#import <mach/mach_host.h>
#import <sys/sysctl.h>

#include <time.h>

#define SYSTEM_VERSION_LESS_THAN(v)                 ([[[UIDevice currentDevice] systemVersion] compare:v options:NSNumericSearch] == NSOrderedAscending)


NSString *personal = @"This copy of QMole is licenced for personal, non commercial use only.\n\n\
In plain English, it means that this copy of QMole is licensed for free for use by private individuals only. This copy of QMole is *NOT* licensed for use in a company, institutional, governmental, organisational or office setting. If you have questions about this, please click \"Contact.\"\n\
\n\
Please support ongoing devolopment and enhancement of QMole through your donantions, if you can. Click \"Donate\" to make a donation. QMole is grateful for your assistance. Thank You. \n\n\
Click \"Start\" to launch QMole...";

NSString *expired = @"This BETA edition of QMole is too old. Please obtain a new Beta or obtain a full release edition of QMole. If you have questions about this, please click \"Contact.\"\n\
\n\
Please support ongoing devolopment and enhancement of QMole through your donantions, if you can. Click \"Donate\" to make a donation. QMole is grateful for your assistance. Thank You.";


NSString *version = @"Welcome to QMole version 0.7 beta\n\n";

NSString *preamble = @"SCROLL DOWN TO ACCEPT THE LICENSE\n\n";

NSString *license = @"By accepting this agreement or by installing QMole (the \"Software\"), you agree to the following terms.\n\n\
The Software is a Linux software interoperability solution and smart routing X11 desktop system for smart phones\
 written by Christoph Kohlhepp and distributed by QMole UK Ltd. If you have any questions about the Software, please click \"Contact.\"\n\
\n\
LICENSE\n\
Subject to your compliance with these terms and conditions, QMole UK Ltd. grants you a royalty-free, non-exclusive,\
 non-transferable license to use the Software, SOLELY FOR YOUR PERSONAL, NON-COMMECIAL PURPOSES. QMole UK Ltd.\
 reserves all rights in the Software not expressly granted to you here.\n\
\n\
RESTRICTIONS\n\
You will not use the Software for illegal purposes. You will comply with all export laws.\
 The Software is licensed, not sold.\n\
\n\
DISCLAIMER OF WARRANTY\n\
QMole UK Ltd. disclaims any responsibility for harm resulting from the Software or any software or content\
 downloaded using the Software, whether or not QMole UK Ltd. approved such software or content.\
 QMole UK Ltd. expressly disclaims all warranties and conditions, express or implied, including\
 any implied warranties and conditions of merchantability, fitness for a particular purpose, and\
 noninfringement, and any warranties and conditions arising out of course of dealing or usage of\
 trade regarding the Software or any software or content you download using the Software. No advice\
 or information, whether oral or written, obtained from QMole UK Ltd. or elsewhere will create any\
 warranty or condition not expressly stated in this agreement.\n\
\n\
LIMITATION OF LIABILITY\n\
In no event and under no theory of liability will QMole UK Ltd. be liable to you for any special,\
 incidental, exemplary, or consequential damages arising out of or in connection with this agreement\
 or the Software whether or not QMole UK Ltd. has been advised of the possibility of such damages.\
 The foregoing limitations will survive even if any limited remedy specified is found to have failed\
 of its essential purpose.\n\
\n\
GENERAL\n\
These QMole UK Ltd. terms will be governed by and construed in accordance with the laws of the United Kingdom,\
 without regard to conflicts of law rules. The United Nations Convention on Contracts for the International\
 Sale of Goods will not apply. The failure by either party to enforce any provision will not constitute a waiver.\
 Any waiver, modification, or amendment of the QMole UK Ltd. terms will be effective only if signed. If any\
 provision is held to be unenforceable, it will be enforced to the maximum extent possible and will not\
 diminish other provisions. QMole UK Ltd. may make changes to these terms from time to time. When these changes are made,\
 QMole UK Ltd. will make a new copy of the terms available at www.qmole.uk/eula. You understand and agree that if you\
 use the Software after the date on which the terms have changed, QMole UK Ltd. will treat your use as acceptance of\
 the updated terms. You agree that QMole UK Ltd. may provide you with notices, including those regarding changes\
 to the terms, by postings on www.qmole.uk/eula. This is QMole UK Ltd.'s complete and exclusive understanding with\
 you regarding your use of the Software as an end user.\n\
\n\
ACKNOWLEDGEMENTS\n\
QMole's primary author is Christoph Kohlhepp.\n\
QMole uses an SDK licensed for royalty-free distribution from ABTO Software.\n\
\n\
OPEN SOURCE ACKNOWLEDGEMENTS\n\
QMole uses in excess of 100 GNU and MIT licensed independent components.\
 In particular QMole would like to mention the following projects:\n\
Herbstluftwm, by Thorsten Wißmann\n\
X11, the X.org Foundation\n\
VNC, AT&T Research Lab\n\
GTK+, The GTK Team\n\
OCaml, INRIA France";

/* #define BETA_TIMEOUT 1451606400 */

/* 31 August 2016 */
#define BETA_TIMEOUT 1472601600

enum {
    kHost,
    kPort,
    kPassword,
};

#define IOS_7_OR_GREATER ([[[UIDevice currentDevice] systemVersion] compare:@"7.0" options:NSNumericSearch] != NSOrderedAscending)

@implementation AuthViewController


- (NSArray *)runningProcesses {
    
    int mib[4] = {CTL_KERN, KERN_PROC, KERN_PROC_ALL, 0};
    size_t miblen = 4;
    
    size_t size;
    int st = sysctl(mib, miblen, NULL, &size, NULL, 0);
    
    struct kinfo_proc * process = NULL;
    struct kinfo_proc * newprocess = NULL;
    
    do {
        
        size += size / 10;
        newprocess = realloc(process, size);
        
        if (!newprocess){
            
            if (process){
                free(process);
            }
            
            return nil;
        }
        
        process = newprocess;
        st = sysctl(mib, miblen, process, &size, NULL, 0);
        
    } while (st == -1 && errno == ENOMEM);
    
    if (st == 0){
        
        if (size % sizeof(struct kinfo_proc) == 0){
            int nprocess = size / sizeof(struct kinfo_proc);
            
            if (nprocess){
                
                NSMutableArray * array = [[NSMutableArray alloc] init];
                
                for (int i = nprocess - 1; i >= 0; i--){
                    
                    NSString * processID = [[NSString alloc] initWithFormat:@"%d", process[i].kp_proc.p_pid];
                    NSString * processName = [[NSString alloc] initWithFormat:@"%s", process[i].kp_proc.p_comm];
                    
                    NSDictionary * dict = [[NSDictionary alloc] initWithObjects:[NSArray arrayWithObjects:processID, processName, nil]
                                                                        forKeys:[NSArray arrayWithObjects:@"ProcessID", @"ProcessName", nil]];
                    [array addObject:dict];
                }
                
                free(process);
                return array;
            }
        }
    }
    
    return nil;
}


-(void)writeToTextFile{
    
    NSArray *paths = NSSearchPathForDirectoriesInDomains
    (NSDocumentDirectory, NSUserDomainMask, YES);
    NSString *documentsDirectory = [paths objectAtIndex:0];
    
    //make a file name to write the data to using the documents directory:
    NSString *fileName = [NSString stringWithFormat:@"%@/qmole_license_accepted.txt",
                          documentsDirectory];
    NSString *content = @"User accepted license OK";
    [content writeToFile:fileName
              atomically:NO
                encoding:NSStringEncodingConversionAllowLossy
                   error:nil];
    
}

-(BOOL)UserAcceptedLicense{
    BOOL ok = NO;
    NSArray *paths = NSSearchPathForDirectoriesInDomains
    (NSDocumentDirectory, NSUserDomainMask, YES);
    NSString *documentsDirectory = [paths objectAtIndex:0];
    NSString *fileName = [NSString stringWithFormat:@"%@/qmole_license_accepted.txt",
                          documentsDirectory];
    NSString *content = [[NSString alloc] initWithContentsOfFile:fileName
                                                    usedEncoding:nil
                                                           error:nil];
    
    if ([content isEqualToString: @"User accepted license OK"])
        ok = YES;
    [content release];
    return ok;
}


-(void)MarkExpired{
    
    NSArray *paths = NSSearchPathForDirectoriesInDomains
    (NSDocumentDirectory, NSUserDomainMask, YES);
    NSString *documentsDirectory = [paths objectAtIndex:0];
    
    //make a file name to write the data to using the documents directory:
    NSString *fileName = [NSString stringWithFormat:@"%@/beta2_now.txt",
                          documentsDirectory];
    NSString *content = @"Ist fertig OK";
    [content writeToFile:fileName
              atomically:NO
                encoding:NSStringEncodingConversionAllowLossy
                   error:nil];
    
}

-(BOOL)IsMarkedExpired{
    BOOL ok = NO;
    NSArray *paths = NSSearchPathForDirectoriesInDomains
    (NSDocumentDirectory, NSUserDomainMask, YES);
    NSString *documentsDirectory = [paths objectAtIndex:0];
    NSString *fileName = [NSString stringWithFormat:@"%@/beta2_now.txt",
                          documentsDirectory];
    NSString *content = [[NSString alloc] initWithContentsOfFile:fileName
                                                    usedEncoding:nil
                                                           error:nil];
    
    if ([content isEqualToString: @"Ist fertig OK"])
        ok = YES;
    [content release];
    return ok;
}


- (NSString *) md5:(NSString *) input
{
    const char *cStr = [input UTF8String];
    unsigned char digest[16];
    CC_MD5( cStr, strlen(cStr), digest ); // This is the md5 call
    
    NSMutableString *output = [NSMutableString stringWithCapacity:CC_MD5_DIGEST_LENGTH * 2];
    
    for(int i = 0; i < CC_MD5_DIGEST_LENGTH; i++)
        [output appendFormat:@"%02x", digest[i]];
    
    return  output;
    
}

- (id)initWithNibName:(NSString *)nibNameOrNil bundle:(NSBundle *)nibBundleOrNil
{
    NSString *configuration = [[[NSBundle mainBundle] infoDictionary] objectForKey:@"Configuration"];

    if ([configuration isEqualToString:@"Debug"])  system("echo initWithNibName start >> /tmp/qmoletrace.log");
    
    if (self = [super initWithNibName:nibNameOrNil bundle:nibBundleOrNil])
    {
        _screenWidth = [[UIScreen mainScreen] bounds].size.width;
        _screenHeight = [[UIScreen mainScreen] bounds].size.height;
        // 5s - 567
        // 4s - 480
        // ipad2 - 1024
        // ipad air - 1024
        
        _rowNames = [NSArray arrayWithObjects:
                     [NSArray arrayWithObjects:
                      NSLocalizedString(@"Host", nil),
                      nil],
                     nil];
        [_rowNames retain];

        //CGFloat aggregateHeight = self.tableView.bounds.size.height;
        
        self.tableView.rowHeight = (_screenHeight - 100) * 0.8;
      
        // host based checksum
        
        char hostname[1024];
        memset(hostname,0,1024);
        gethostname(hostname, 1024);
        NSString *nsHostname = [NSString stringWithCString:hostname encoding:NSUTF8StringEncoding];
        NSString *nsCksum = [[self md5:nsHostname] substringToIndex:8];
        
        NSString*  debugcmd = [NSString stringWithFormat:@"echo access checksum: %@ >> /tmp/qmole.log &",
                               nsCksum];
        system([debugcmd cStringUsingEncoding:NSASCIIStringEncoding]);
        
        
        _server = [[RSServer alloc] init];
        _server.host = @"localhost";
        _server.port = 5900;
        if ([nsHostname compare:@"fergus"]==NSOrderedSame)
            _server.password = @"alabama";
        else
            _server.password = nsCksum;
        _server.display = 0;
        _server.colorMode = RSColorRGB32;
        
        _server.encodings = [NSArray arrayWithObjects:
                             [NSNumber numberWithInt:RSEncodingTight],
                             nil];
        
        _server.compressionLevel = 6;
        _server.qualityLevel = 6;
        
        
    }
    
    if ([configuration isEqualToString:@"Debug"]) system("echo initWithNibName end >> /tmp/qmoletrace.log");
    
    return self;
}

- (void)dealloc
{
    [_rowNames release];
    [_server release];
    [super dealloc];
}

- (void)Recover
{
    NSString *configuration = [[[NSBundle mainBundle] infoDictionary] objectForKey:@"Configuration"];
    
    if ([configuration isEqualToString:@"Debug"]) system("echo Recover start >> /tmp/qmoletrace.log");
    
    system("echo Initiate QMole service recovery >> /tmp/qmole.log");
    if ([self isQMoleRunning]){
        system("echo QMole services are running >> /tmp/qmole.log");
        
    } else {
        system("echo QMole services are NOT running; starting >> /tmp/qmole.log");
        system("/usr/bin/qmole-ctrl >> /tmp/qmole.log &");
    }
    
    if ([configuration isEqualToString:@"Debug"]) system("echo Recover end >> /tmp/qmoletrace.log");
    
}

- (BOOL)isQMoleRunning
{
    NSString *configuration = [[[NSBundle mainBundle] infoDictionary] objectForKey:@"Configuration"];
    
    if ([configuration isEqualToString:@"Debug"]) system("echo isQMoleRunning start >> /tmp/qmoletrace.log");
    
    BOOL isVncRunning = NO;   /* Xvnc */
    BOOL isDzenRunning = NO;  /* dzen2 */
    BOOL isHerbstluftRunning = NO; /* herbstluftwm */
    BOOL isBmpanelRunning = NO; /* bmpanel2 */
    
    NSArray *ps = [self runningProcesses];
    for (NSDictionary *dict in ps) {
        
        NSString *psname = [dict objectForKey:@"ProcessName"];
        //NSString *pspid = [dict objectForKey:@"ProcessID"];
        
        if ([psname compare:@"Xvnc"]==NSOrderedSame){
            isVncRunning = YES;
        }
        if ([psname compare:@"dzen2"]==NSOrderedSame){
            isDzenRunning = YES;
        }
        if ([psname compare:@"herbstluftwm"]==NSOrderedSame){
            isHerbstluftRunning = YES;
        }
        if ([psname compare:@"bmpanel2"]==NSOrderedSame){
            isBmpanelRunning = YES;
        }
        //if ([psname rangeOfString:@"wsbar.sh"].location != NSNotFound){
        //    isWsbarRunning = YES;
        //    NSString*  debugcmd = [NSString stringWithFormat:@"echo Process: %@ >> /tmp/qmole.log &",
        //                           psname];
        //    system([debugcmd cStringUsingEncoding:NSASCIIStringEncoding]);
        //
        //}
        
    }
    if ([configuration isEqualToString:@"Debug"]) system("echo isQMoleRunning end >> /tmp/qmoletrace.log");
    
    if (isVncRunning)
        return YES;
    return NO;
}



- (void)viewDidLoad
{
    NSString *configuration = [[[NSBundle mainBundle] infoDictionary] objectForKey:@"Configuration"];
    
    if ([configuration isEqualToString:@"Debug"]) system("echo viewDidLoad start >> /tmp/qmoletrace.log");
    
    system("echo QMole Started > /tmp/qmole.log");
    
    [self Recover];
    
    [super viewDidLoad];

    [self setTitle:NSLocalizedString(@"QMole", nil)];
    
    _licenseAccepted = NO;
 
    if ([configuration isEqualToString:@"Debug"]) system("echo viewDidLoad end >> /tmp/qmoletrace.log");
}

- (void)viewDidAppear:(BOOL)animated
{
    NSString *configuration = [[[NSBundle mainBundle] infoDictionary] objectForKey:@"Configuration"];
    
    time_t current_time;
    current_time = time(NULL);
    char* c_time_string;
    c_time_string = ctime(&current_time);
    
    
    
    // http://www.epochconverter.com/epoch/timestamp-list.php
    
    if ([configuration isEqualToString:@"Debug"])  system("echo viewDidAppear start >> /tmp/qmoletrace.log");
    
    [super viewDidAppear:animated];
    
    if ([configuration isEqualToString:@"Debug"])  system("echo viewDidAppear step1 >> /tmp/qmoletrace.log");
    
    [[UIApplication sharedApplication] setStatusBarHidden:NO];
    [self.navigationController setNavigationBarHidden:NO animated:animated];
    [self.navigationController setToolbarHidden:YES animated:YES];

    if ([configuration isEqualToString:@"Debug"]) system("echo viewDidAppear step2 >> /tmp/qmoletrace.log");
    
    // Copy pasted from ConnectToServer
    NSString* nibName = [[UIDevice currentDevice] userInterfaceIdiom] == UIUserInterfaceIdiomPhone ? @"RemoteScreenViewController_iPhone" : @"RemoteScreenViewController_iPad";
    RemoteScreenViewController *controller = [[[RemoteScreenViewController alloc] initWithNibName:nibName bundle:nil] autorelease];
    
    controller.server = _server;

    if ([configuration isEqualToString:@"Debug"]) system("echo viewDidAppear step3 >> /tmp/qmoletrace.log");
    
    _screenWidth = [[UIScreen mainScreen] bounds].size.width;
    CGFloat buttonwidth = 80;
    CGFloat gap = (_screenWidth - (3 * buttonwidth)) /5;
    
    if ([configuration isEqualToString:@"Debug"])  system("echo viewDidAppear step4 >> /tmp/qmoletrace.log");
    
    _accept = [UIButton buttonWithType:UIButtonTypeRoundedRect];
    [_accept addTarget:self action:@selector(connectToServer:)
     forControlEvents:UIControlEventTouchUpInside];
    if (_screenHeight < 768){
        _accept.enabled = NO;
    } else {
        
        if ((current_time < BETA_TIMEOUT) && (![self IsMarkedExpired])){
            _accept.enabled = YES;
        }
    }
    if ([self UserAcceptedLicense]){
        [_accept setTitle:@"Start" forState:UIControlStateNormal];
        if ((current_time < BETA_TIMEOUT) && (![self IsMarkedExpired])){
            _accept.enabled = YES;
        }
    } else {
        [_accept setTitle:@"I Agree" forState:UIControlStateNormal];
    }
    _accept.frame = CGRectMake(gap, _screenHeight - 60.0, buttonwidth, 40.0);
    
    if ([configuration isEqualToString:@"Debug"])  system("echo viewDidAppear step5 >> /tmp/qmoletrace.log");
    
    
    _donate = [UIButton buttonWithType:UIButtonTypeSystem];
    [_donate addTarget:self action:@selector(donateNow:)
      forControlEvents:UIControlEventTouchUpInside];
    
    if ([configuration isEqualToString:@"Debug"])  system("echo viewDidAppear step6 >> /tmp/qmoletrace.log");
    
    
    if (SYSTEM_VERSION_LESS_THAN(@"7.0.2")) {
        [_donate setTitle:@"Donate $" forState:UIControlStateNormal];
    } else {
        [_donate setImage: [[UIImage imageNamed: @"donate.png"] imageWithRenderingMode: UIImageRenderingModeAlwaysOriginal] forState: UIControlStateNormal];
    }

    if ([configuration isEqualToString:@"Debug"]) system("echo viewDidAppear step7 >> /tmp/qmoletrace.log");
    
    
    _donate.frame = CGRectMake(gap*2 + buttonwidth, _screenHeight - 60.0, buttonwidth, 40.0);
    _donate.enabled = YES;
    
    if ([configuration isEqualToString:@"Debug"]) system("echo viewDidAppear step8 >> /tmp/qmoletrace.log");
    
    
    _contact = [UIButton buttonWithType:UIButtonTypeRoundedRect];
    [_contact addTarget:self action:@selector(connectToContact:)
      forControlEvents:UIControlEventTouchUpInside];
    [_contact setTitle:@"Contact" forState:UIControlStateNormal];
    _contact.frame = CGRectMake(gap*3 + buttonwidth*2, _screenHeight - 60.0, buttonwidth, 40.0);
    _contact.enabled = YES;
    
    if ([configuration isEqualToString:@"Debug"]) system("echo viewDidAppear step9 >> /tmp/qmoletrace.log");
    

    [self.navigationController.view addSubview:_accept];
    [self.navigationController.view addSubview:_donate];
    [self.navigationController.view addSubview:_contact];
    
    if ([configuration isEqualToString:@"Debug"]) system("echo viewDidAppear step10 >> /tmp/qmoletrace.log");

    if ((current_time > BETA_TIMEOUT) || ([self IsMarkedExpired])){
        _accept.enabled = NO;
        if (![self IsMarkedExpired]){
            [self MarkExpired];
        }
    } else {
        if (_licenseAccepted)
            [self connectToServer:0];
    }

    if ([configuration isEqualToString:@"Debug"]) system("echo viewDidAppear end >> /tmp/qmoletrace.log");
    
}

- (BOOL)shouldAutorotateToInterfaceOrientation:(UIInterfaceOrientation)interfaceOrientation
{
    //if ([[UIDevice currentDevice] userInterfaceIdiom] == UIUserInterfaceIdiomPhone)
    //    return (interfaceOrientation != UIInterfaceOrientationPortraitUpsideDown);
    //else
    //    return YES;
    return NO;
}

#pragma mark - Table data

- (NSInteger)numberOfSectionsInTableView:(UITableView *)tableView
{
    return 1;
}

- (NSString *)tableView:(UITableView *)tableView titleForHeaderInSection:(NSInteger)section
{
    return NSLocalizedString(@"QMole Free Personal Use License", nil);
}

- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section
{
    return [[_rowNames objectAtIndex:section] count];
}

- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath
{
    NSString *configuration = [[[NSBundle mainBundle] infoDictionary] objectForKey:@"Configuration"];
    
    if ([configuration isEqualToString:@"Debug"])  system("echo cellForRowAtIndexPath start >> /tmp/qmoletrace.log");
    
    static NSString *kCellIdentifier = @"cellID";
 
    if ([configuration isEqualToString:@"Debug"])  system("echo cellForRowAtIndexPath step1 >> /tmp/qmoletrace.log");
    
	UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier:kCellIdentifier];
	if (!cell)
	{
		cell = [[[UITableViewCell alloc] initWithStyle:UITableViewCellStyleSubtitle reuseIdentifier:kCellIdentifier] autorelease];
    }

    if ([configuration isEqualToString:@"Debug"])  system("echo cellForRowAtIndexPath step2 >> /tmp/qmoletrace.log");
    
    //CGFloat rowHeight = tableView.rowHeight;
    //rowHeight = (IOS_7_OR_GREATER && (rowHeight == UITableViewAutomaticDimension)) ? 53 : rowHeight/2;
    //CGFloat viewWidth = cell.bounds.size.width;
    CGFloat viewWidth2 = _screenWidth - 50;
    if (SYSTEM_VERSION_LESS_THAN(@"7.0.2")) {
        viewWidth2 = _screenWidth - 80;
    }
    
    if ([configuration isEqualToString:@"Debug"])  system("echo cellForRowAtIndexPath step3 >> /tmp/qmoletrace.log");
    
    _myTextView = [[UITextView alloc] initWithFrame:CGRectMake(0, 0, viewWidth2, self.tableView.rowHeight)];
    if ([self UserAcceptedLicense]){
        
        time_t current_time;
        current_time = time(NULL);
        char* c_time_string;
        c_time_string = ctime(&current_time);
        
        if ([configuration isEqualToString:@"Debug"]) system("echo cellForRowAtIndexPath step4a >> /tmp/qmoletrace.log");
        
        if ((current_time > BETA_TIMEOUT) || ([self IsMarkedExpired])){
            _myTextView.text = [NSString stringWithFormat:@"%@%@", version,expired];
        } else {
            _myTextView.text = [NSString stringWithFormat:@"%@%@", version,personal];
        }
        
        
    } else {
        if ([configuration isEqualToString:@"Debug"]) system("echo cellForRowAtIndexPath step4b >> /tmp/qmoletrace.log");
        
        if (_screenHeight < 768){
            _myTextView.text = [NSString stringWithFormat:@"%@%@%@", version,preamble,license];
        } else {
            _myTextView.text = [NSString stringWithFormat:@"%@%@", version,license];
        }
    }
    
    if ([configuration isEqualToString:@"Debug"]) system("echo cellForRowAtIndexPath step5 >> /tmp/qmoletrace.log");

    [_myTextView setScrollEnabled:YES];
    
    if ([configuration isEqualToString:@"Debug"])  system("echo cellForRowAtIndexPath step6 >> /tmp/qmoletrace.log");
    
    
    [_myTextView setUserInteractionEnabled:YES];
    
    if ([configuration isEqualToString:@"Debug"]) system("echo cellForRowAtIndexPath step7 >> /tmp/qmoletrace.log");
    
    
    [_myTextView setFont:[UIFont fontWithName:@"Arial" size:12]];
    
    if ([configuration isEqualToString:@"Debug"]) system("echo cellForRowAtIndexPath step8 >> /tmp/qmoletrace.log");
    
    _myTextView.delegate = self;
    
    if ([configuration isEqualToString:@"Debug"]) system("echo cellForRowAtIndexPath step9 >> /tmp/qmoletrace.log");

    
    [cell.contentView addSubview:_myTextView];
   
    if ([configuration isEqualToString:@"Debug"]) system("echo cellForRowAtIndexPath end >> /tmp/qmoletrace.log");
    
    return cell;
}

#pragma mark - Table navigation

- (void)tableView:(UITableView *)tableView didSelectRowAtIndexPath:(NSIndexPath *)indexPath
{
    [tableView deselectRowAtIndexPath:indexPath animated:YES];
    UITextField *textField = (UITextField *)[self.tableView cellForRowAtIndexPath:indexPath].accessoryView;
    [textField becomeFirstResponder];
}

#pragma mark - Text view delegate method stubs

- (void)textViewDidBeginEditing:(UITextView *)textView {
}

- (BOOL)textView:(UITextView *)textView shouldChangeTextInRange:(NSRange)range replacementText:(NSString *)text {
    return NO;
}

- (void)scrollViewDidScroll:(UIScrollView *)scrollView {
    
    time_t current_time;
    current_time = time(NULL);
    char* c_time_string;
    c_time_string = ctime(&current_time);
    
    if ((current_time > BETA_TIMEOUT) || ([self IsMarkedExpired]))
        return;
    
    if (scrollView.contentOffset.y > 0){
        if (scrollView.contentOffset.y >= scrollView.contentSize.height - scrollView.frame.size.height)
        {
            _accept.enabled = YES;
        }
    }
}

#pragma mark - Remote session

- (IBAction)connectToContact:(id)sender
{
    NSString *recipients = @"mailto:support@qmole.uk?subject=QMole Contact";
    NSString *body = @"&body=";
    
    NSString *email = [NSString stringWithFormat:@"%@%@", recipients, body];
    email = [email stringByAddingPercentEscapesUsingEncoding:NSUTF8StringEncoding];
    
    [[UIApplication sharedApplication] openURL:[NSURL URLWithString:email]];
}


- (IBAction)donateNow:(id)sender
{
    [[UIApplication sharedApplication] openURL:[NSURL URLWithString:@"http://www.qmole.uk/donations"]];
}

- (IBAction)connectToServer:(id)sender
{
    NSString *configuration = [[[NSBundle mainBundle] infoDictionary] objectForKey:@"Configuration"];
    
    if ([configuration isEqualToString:@"Debug"]) system("echo connectToServer start >> /tmp/qmoletrace.log");
    
    [self Recover];
    
    _licenseAccepted = YES;
    _myTextView.text = @"\n\n\n\n    QMole screen reset..."; // For next time around...
    
     [self writeToTextFile];
    _accept.hidden = YES;
    _donate.hidden = YES;
    _contact.hidden = YES;
    NSString* nibName = [[UIDevice currentDevice] userInterfaceIdiom] == UIUserInterfaceIdiomPhone ? @"RemoteScreenViewController_iPhone" : @"RemoteScreenViewController_iPad";
    RemoteScreenViewController *controller = [[[RemoteScreenViewController alloc] initWithNibName:nibName bundle:nil] autorelease];
    
    controller.server = _server;
  
    CATransition* transition = [CATransition animation];
    transition.duration = 4;
    transition.type = kCATransitionFade;
    transition.subtype = kCATransitionFromTop;
    
    [self.navigationController.view.layer addAnimation:transition forKey:kCATransition];
    [self.navigationController pushViewController:controller animated:NO];
    
    if ([configuration isEqualToString:@"Debug"]) system("echo connectToServer end >> /tmp/qmoletrace.log");
    
}

@end
