//
//  Copyright (C) 2001-2004 HorizonLive.com, Inc.  All Rights Reserved.
//  Copyright (C) 2002 Constantin Kaplinsky.  All Rights Reserved.
//  Copyright (C) 1999 AT&T Laboratories Cambridge.  All Rights Reserved.
//
//  This is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  This software is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this software; if not, write to the Free Software
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307,
//  USA.
//

//
// VncViewer.java - the VNC viewer applet.  This class mainly just sets up the
// user interface, leaving it to the VncCanvas to do the actual rendering of
// a VNC desktop.
//

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.net.*;
import java.util.*;

public class VncViewer extends java.applet.Applet
  implements java.lang.Runnable, WindowListener {

  boolean inAnApplet = true;
  boolean inSeparateFrame = false;

  //
  // main() is called when run as a java program from the command line.
  // It simply runs the applet inside a newly-created frame.
  //

  public static void main(String[] argv) {
    VncViewer v = new VncViewer();
    v.mainArgs = argv;
    v.inAnApplet = false;
    v.inSeparateFrame = true;

    v.init();
    v.start();
  }

  String[] mainArgs;

  RfbProto rfb;
  Thread rfbThread;

  Frame vncFrame;
  Container vncContainer;
  ScrollPane desktopScrollPane;
  GridBagLayout gridbag;
  ButtonPanel buttonPanel;
  Label connStatusLabel;
  AuthPanel authenticator;
  AuthUnixLoginPanel authenticatorUnixLogin;
  VncCanvas vc;
  OptionsFrame options;
  ClipboardFrame clipboard;
  RecordingFrame rec;

  // Control session recording.
  Object recordingSync;
  String sessionFileName;
  boolean recordingActive;
  boolean recordingStatusChanged;
  String cursorUpdatesDef;
  String eightBitColorsDef;

  // Variables read from parameter values.
  String socketFactory;
  String host;
  int port, vncserverport;
  boolean showControls;
  boolean offerRelogin;
  boolean showOfflineDesktop;
  int deferScreenUpdates;
  int deferCursorUpdates;
  int deferUpdateRequests;

  boolean disableSSL;
  boolean GET;
  String CONNECT;
  String urlPrefix;
  String httpsPort;
  String oneTimeKey;
  String serverCert;
  String proxyHost;
  String proxyPort;
  boolean forceProxy;
  boolean ignoreProxy;
  boolean trustAllVncCerts;
  boolean trustUrlVncCert;
  boolean debugCerts;
  boolean debugKeyboard;
  boolean mapF5_to_atsign;
  boolean forbid_Ctrl_Alt;

  // Reference to this applet for inter-applet communication.
  public static java.applet.Applet refApplet;

  //
  // init()
  //

  public void init() {

    readParameters();

    refApplet = this;

    if (inSeparateFrame) {
      vncFrame = new Frame("TightVNC");
      if (!inAnApplet) {
	vncFrame.add("Center", this);
      }
      vncContainer = vncFrame;
    } else {
      vncContainer = this;
    }

    recordingSync = new Object();

    options = new OptionsFrame(this);
    clipboard = new ClipboardFrame(this);
    authenticator = new AuthPanel(this);
    authenticatorUnixLogin = new AuthUnixLoginPanel();
    if (RecordingFrame.checkSecurity())
      rec = new RecordingFrame(this);

    sessionFileName = null;
    recordingActive = false;
    recordingStatusChanged = false;
    cursorUpdatesDef = null;
    eightBitColorsDef = null;

    if (inSeparateFrame)
      vncFrame.addWindowListener(this);

    rfbThread = new Thread(this);
    rfbThread.start();
  }

  public void update(Graphics g) {
  }

  //
  // run() - executed by the rfbThread to deal with the RFB socket.
  //

  public void run() {

    gridbag = new GridBagLayout();
    vncContainer.setLayout(gridbag);

    GridBagConstraints gbc = new GridBagConstraints();
    gbc.gridwidth = GridBagConstraints.REMAINDER;
    gbc.anchor = GridBagConstraints.NORTHWEST;

    if (showControls) {
      buttonPanel = new ButtonPanel(this);
      gridbag.setConstraints(buttonPanel, gbc);
      vncContainer.add(buttonPanel);
    }

    try {
      connectAndAuthenticate();
      doProtocolInitialisation();

      vc = new VncCanvas(this);
      gbc.weightx = 1.0;
      gbc.weighty = 1.0;

      if (inSeparateFrame) {

	// Create a panel which itself is resizeable and can hold
	// non-resizeable VncCanvas component at the top left corner.
	Panel canvasPanel = new Panel();
	canvasPanel.setLayout(new FlowLayout(FlowLayout.LEFT, 0, 0));
	canvasPanel.add(vc);

	// Create a ScrollPane which will hold a panel with VncCanvas
	// inside.
	desktopScrollPane = new ScrollPane(ScrollPane.SCROLLBARS_AS_NEEDED);
	gbc.fill = GridBagConstraints.BOTH;
	gridbag.setConstraints(desktopScrollPane, gbc);
	desktopScrollPane.add(canvasPanel);

	// Finally, add our ScrollPane to the Frame window.
	vncFrame.add(desktopScrollPane);
	vncFrame.setTitle(rfb.desktopName);
	vncFrame.pack();
	vc.resizeDesktopFrame();

      } else {

	// Just add the VncCanvas component to the Applet.
	gridbag.setConstraints(vc, gbc);
	add(vc);
	validate();

      }

      if (showControls)
	buttonPanel.enableButtons();

      moveFocusToDesktop();
      processNormalProtocol();

    } catch (NoRouteToHostException e) {
      fatalError("Network error: no route to server: " + host, e);
    } catch (UnknownHostException e) {
      fatalError("Network error: server name unknown: " + host, e);
    } catch (ConnectException e) {
      fatalError("Network error: could not connect to server: " +
		 host + ":" + port, e);
    } catch (EOFException e) {
      if (showOfflineDesktop) {
	e.printStackTrace();
	System.out.println("Network error: remote side closed connection");
	if (vc != null) {
	  vc.enableInput(false);
	}
	if (inSeparateFrame) {
	  vncFrame.setTitle(rfb.desktopName + " [disconnected]");
	}
	if (rfb != null && !rfb.closed())
	  rfb.close();
	if (showControls && buttonPanel != null) {
	  buttonPanel.disableButtonsOnDisconnect();
	  if (inSeparateFrame) {
	    vncFrame.pack();
	  } else {
	    validate();
	  }
	}
      } else {
	fatalError("Network error: remote side closed connection", e);
      }
    } catch (IOException e) {
      String str = e.getMessage();
      if (str != null && str.length() != 0) {
	fatalError("Network Error: " + str, e);
      } else {
	fatalError(e.toString(), e);
      }
    } catch (Exception e) {
      String str = e.getMessage();
      if (str != null && str.length() != 0) {
	fatalError("Error: " + str, e);
      } else {
	fatalError(e.toString(), e);
      }
    }
    
  }


  //
  // Process RFB socket messages.
  // If the rfbThread is being stopped, ignore any exceptions,
  // otherwise rethrow the exception so it can be handled.
  //
 
  void processNormalProtocol() throws Exception {
    try {
      vc.processNormalProtocol();
    } catch (Exception e) {
      if (rfbThread == null) {
	System.out.println("Ignoring RFB socket exceptions" +
			   " because applet is stopping");
      } else {
	throw e;
      }
    }
  }


  //
  // Connect to the RFB server and authenticate the user.
  //

  void connectAndAuthenticate() throws Exception
  {
    showConnectionStatus("Initializing...");
    if (inSeparateFrame) {
      vncFrame.pack();
      vncFrame.show();
    } else {
      validate();
    }

	if (false) {
		/* a bug on retries: 'Error: bad position: 8' sun.awt.X11.XTextFieldPeer.setCaretPosition(XTextFieldPeer.java:215) */
		while (!tryAuthenticate()) {
			authenticator.retry();
			authenticatorUnixLogin.retry();
		}
	} else {
		/* just try once and not forever... */
		if (!tryAuthenticate()) {
    			showConnectionStatus("Authentication Failed.");
			showMessage("Authentication Failed.");
			if (!offerRelogin) {
				fatalError("auth failed.");
			}
		} else {
			//showConnectionStatus("Authentication OK.");
		}
	}
  }


  //
  // Try to connect and authenticate.
  //

  boolean tryAuthenticate() throws Exception
  {
    showConnectionStatus("Connecting to " + host + ", port " + port + "...");

    rfb = new RfbProto(host, port, this);
    showConnectionStatus("Connected to server");

    rfb.readVersionMsg();
    showConnectionStatus("RFB server supports protocol version " +
			 rfb.serverMajor + "." + rfb.serverMinor);

    rfb.writeVersionMsg();
    showConnectionStatus("Using RFB protocol version " +
			 rfb.clientMajor + "." + rfb.clientMinor);

    int secType = rfb.negotiateSecurity();
    int authType = 0;

    // FIXME: Map security types to authentication schemes in RfbProto.
    switch (secType) {
    case RfbProto.SecTypeNone:
      authType = RfbProto.AuthNone;
      break;
    case RfbProto.SecTypeVncAuth:
      authType = RfbProto.AuthVNC;
      break;
    case RfbProto.SecTypeTight:
      showConnectionStatus("Enabling TightVNC protocol extensions");
      rfb.initCapabilities();
      rfb.setupTunneling();
      authType = rfb.negotiateAuthenticationTight();
      break;
    default:
      throw new Exception("Unknown security type " + secType);
    }

    boolean success = false;

    switch (authType) {
    case RfbProto.AuthNone:
      showConnectionStatus("No authentication needed");
      success = true;
      break;
    case RfbProto.AuthVNC:
      showConnectionStatus("Performing standard VNC authentication");
      if (authenticator.isInteractionNecessary()) {
	showAuthPanel(authenticator);
	authenticator.moveFocusToDefaultField();
      }
      success = authenticator.tryAuthenticate(rfb);
      if (authenticator.isInteractionNecessary()) {
	vncContainer.remove(authenticator);
      } else {
	// Don't retry non-interactive authentication.
	if (!success)
	  throw new Exception("VNC authentication failed");
      }
      break;
    case RfbProto.AuthUnixLogin:
      showConnectionStatus("Performing Unix login-style authentication");
      showAuthPanel(authenticatorUnixLogin);
      authenticatorUnixLogin.moveFocusToDefaultField();
      success = authenticatorUnixLogin.tryAuthenticate(rfb);
      vncContainer.remove(authenticatorUnixLogin);
      break;
    default:
      throw new Exception("Unknown authentication scheme " + authType);
    }

    if (!success)
      rfb.close();

    return success;
  }


  //
  // Show a message describing the connection status.
  // To hide the connection status label, use (msg == null).
  //

  void showConnectionStatus(String msg)
  {
    if (msg == null) {
      if (vncContainer.isAncestorOf(connStatusLabel)) {
	vncContainer.remove(connStatusLabel);
      }
      return;
    }

    System.out.println(msg);

    if (connStatusLabel == null) {
      connStatusLabel = new Label("Status: " + msg);
      connStatusLabel.setFont(new Font("Helvetica", Font.PLAIN, 12));
    } else {
      connStatusLabel.setText("Status: " + msg);
    }

    if (!vncContainer.isAncestorOf(connStatusLabel)) {
      GridBagConstraints gbc = new GridBagConstraints();
      gbc.gridwidth = GridBagConstraints.REMAINDER;
      gbc.fill = GridBagConstraints.HORIZONTAL;
      gbc.anchor = GridBagConstraints.NORTHWEST;
      gbc.weightx = 1.0;
      gbc.weighty = 1.0;
      gbc.insets = new Insets(20, 30, 20, 30);
      gridbag.setConstraints(connStatusLabel, gbc);
      vncContainer.add(connStatusLabel);
    }

    if (inSeparateFrame) {
      vncFrame.pack();
    } else {
      validate();
    }
  }


  //
  // Show an authentication panel.
  //

  void showAuthPanel(Panel authPanel)
  {
    showConnectionStatus(null);

    GridBagConstraints gbc = new GridBagConstraints();
    gbc.gridwidth = GridBagConstraints.REMAINDER;
    gbc.anchor = GridBagConstraints.NORTHWEST;
    gbc.weightx = 1.0;
    gbc.weighty = 1.0;
    gbc.ipadx = 100;
    gbc.ipady = 50;
    gridbag.setConstraints(authPanel, gbc);
    try {
    vncContainer.add(authPanel);
    } catch (Exception e) {
    }

    if (inSeparateFrame) {
      vncFrame.pack();
    } else {
      validate();
    }
  }


  //
  // Do the rest of the protocol initialisation.
  //

  void doProtocolInitialisation() throws IOException
  {
    rfb.writeClientInit();
    rfb.readServerInit();

    System.out.println("Desktop name is " + rfb.desktopName);
    System.out.println("Desktop size is " + rfb.framebufferWidth + " x " +
		       rfb.framebufferHeight);

    setEncodings();

    showConnectionStatus(null);
  }


  //
  // Send current encoding list to the RFB server.
  //

  void setEncodings() {
    try {
      if (rfb != null && rfb.inNormalProtocol) {
	rfb.writeSetEncodings(options.encodings, options.nEncodings);
	if (vc != null) {
	  vc.softCursorFree();
	}
      }
    } catch (Exception e) {
      e.printStackTrace();
    }
  }


  //
  // setCutText() - send the given cut text to the RFB server.
  //

  void setCutText(String text) {
    try {
      if (rfb != null && rfb.inNormalProtocol) {
	rfb.writeClientCutText(text);
      }
    } catch (Exception e) {
      e.printStackTrace();
    }
  }


  //
  // Order change in session recording status. To stop recording, pass
  // null in place of the fname argument.
  //

  void setRecordingStatus(String fname) {
    synchronized(recordingSync) {
      sessionFileName = fname;
      recordingStatusChanged = true;
    }
  }

  //
  // Start or stop session recording. Returns true if this method call
  // causes recording of a new session.
  //

  boolean checkRecordingStatus() throws IOException {
    synchronized(recordingSync) {
      if (recordingStatusChanged) {
	recordingStatusChanged = false;
	if (sessionFileName != null) {
	  startRecording();
	  return true;
	} else {
	  stopRecording();
	}
      }
    }
    return false;
  }

  //
  // Start session recording.
  //

  protected void startRecording() throws IOException {
    synchronized(recordingSync) {
      if (!recordingActive) {
	// Save settings to restore them after recording the session.
	cursorUpdatesDef =
	  options.choices[options.cursorUpdatesIndex].getSelectedItem();
	eightBitColorsDef =
	  options.choices[options.eightBitColorsIndex].getSelectedItem();
	// Set options to values suitable for recording.
	options.choices[options.cursorUpdatesIndex].select("Disable");
	options.choices[options.cursorUpdatesIndex].setEnabled(false);
	options.setEncodings();
	options.choices[options.eightBitColorsIndex].select("No");
	options.choices[options.eightBitColorsIndex].setEnabled(false);
	options.setColorFormat();
      } else {
	rfb.closeSession();
      }

      System.out.println("Recording the session in " + sessionFileName);
      rfb.startSession(sessionFileName);
      recordingActive = true;
    }
  }

  //
  // Stop session recording.
  //

  protected void stopRecording() throws IOException {
    synchronized(recordingSync) {
      if (recordingActive) {
	// Restore options.
	options.choices[options.cursorUpdatesIndex].select(cursorUpdatesDef);
	options.choices[options.cursorUpdatesIndex].setEnabled(true);
	options.setEncodings();
	options.choices[options.eightBitColorsIndex].select(eightBitColorsDef);
	options.choices[options.eightBitColorsIndex].setEnabled(true);
	options.setColorFormat();

	rfb.closeSession();
	System.out.println("Session recording stopped.");
      }
      sessionFileName = null;
      recordingActive = false;
    }
  }


  //
  // readParameters() - read parameters from the html source or from the
  // command line.  On the command line, the arguments are just a sequence of
  // param_name/param_value pairs where the names and values correspond to
  // those expected in the html applet tag source.
  //

  public void readParameters() {
    host = readParameter("HOST", !inAnApplet);
    if (host == null) {
      host = getCodeBase().getHost();
      if (host.equals("")) {
	fatalError("HOST parameter not specified");
      }
    }
    Date d = new Date();
    System.out.println("-\nSSL VNC Java Applet starting.  " + d);

    port = 0;
    String str = readParameter("PORT", false);
    if (str != null) {
	port = Integer.parseInt(str);
    }
    // When there is a proxy VNCSERVERPORT may be inaccessible (inside firewall).
    vncserverport = 0;
    str = readParameter("VNCSERVERPORT", false);
    if (str != null) {
	vncserverport = Integer.parseInt(str);
    }
    if (port == 0 && vncserverport == 0) {
	fatalError("Neither PORT nor VNCSERVERPORT parameters specified");
    }
    if (port == 0) {
	// Nevertheless, fall back to vncserverport if we have to.
	System.out.println("using vncserverport: '" + vncserverport + "' for PORT.");
	port = vncserverport;
    }

    if (inAnApplet) {
      str = readParameter("Open New Window", false);
      if (str != null && str.equalsIgnoreCase("Yes"))
	inSeparateFrame = true;
    }

    // "Show Controls" set to "No" disables button panel.
    showControls = true;
    str = readParameter("Show Controls", false);
    if (str != null && str.equalsIgnoreCase("No"))
      showControls = false;

    // "Offer Relogin" set to "No" disables "Login again" and "Close
    // window" buttons under error messages in applet mode.
    offerRelogin = true;
    str = readParameter("Offer Relogin", false);
    if (str != null && str.equalsIgnoreCase("No"))
      offerRelogin = false;

    // Do we continue showing desktop on remote disconnect?
    showOfflineDesktop = false;
    str = readParameter("Show Offline Desktop", false);
    if (str != null && str.equalsIgnoreCase("Yes"))
      showOfflineDesktop = true;

    // Fine tuning options.
    deferScreenUpdates = readIntParameter("Defer screen updates", 20);
    deferCursorUpdates = readIntParameter("Defer cursor updates", 10);
    deferUpdateRequests = readIntParameter("Defer update requests", 50);

    // SocketFactory.
    socketFactory = readParameter("SocketFactory", false);

    // SSL
    disableSSL = false;
    str = readParameter("DisableSSL", false);
    if (str != null && str.equalsIgnoreCase("Yes"))
      disableSSL = true;

    httpsPort = readParameter("httpsPort", false);

    // Extra GET, CONNECT string:
    CONNECT = readParameter("CONNECT", false);
    if (CONNECT != null) {
	CONNECT = CONNECT.replaceAll(" ", ":");
    }

    GET = false;
    str = readParameter("GET", false);
    if (str != null && str.equalsIgnoreCase("Yes")) {
      GET = true;
    }
    if (str != null && str.equalsIgnoreCase("1")) {
      GET = true;
    }

    urlPrefix = readParameter("urlPrefix", false);
    if (urlPrefix != null) {
	urlPrefix = urlPrefix.replaceAll("%2F", "/");
	urlPrefix = urlPrefix.replaceAll("%2f", "/");
	urlPrefix = urlPrefix.replaceAll("_2F_", "/");
	if (urlPrefix.indexOf("/") != 0) {
		urlPrefix = "/" + urlPrefix;
	}
    } else {
    	urlPrefix = "";
    }
    System.out.println("urlPrefix: '" + urlPrefix + "'");

    oneTimeKey = readParameter("oneTimeKey", false);
    if (oneTimeKey != null) {
    	System.out.println("oneTimeKey is set.");
    }

    serverCert = readParameter("serverCert", false);
    if (serverCert != null) {
    	System.out.println("serverCert is set.");
    }

    forceProxy = false;
    proxyHost = null;
    proxyPort = null;
    str = readParameter("forceProxy", false);
    if (str != null) {
	    if (str.equalsIgnoreCase("Yes")) {
		forceProxy = true;
	    } else if (str.equalsIgnoreCase("No")) {
		forceProxy = false;
	    } else {
		forceProxy = true;
		String[] pieces = str.split(" ");
		proxyHost = new String(pieces[0]);
		if (pieces.length >= 2) {
			proxyPort = new String(pieces[1]);
		} else {
			proxyPort = new String("8080");
		}
	    }
    }
    str = readParameter("proxyHost", false);
    if (str != null) {
	proxyHost = new String(str);
    }
    str = readParameter("proxyPort", false);
    if (str != null) {
	proxyPort = new String(str);
    }
    if (proxyHost != null && proxyPort == null) {
    	proxyPort = new String("8080");
    }

    ignoreProxy = false;
    str = readParameter("ignoreProxy", false);
    if (str != null && str.equalsIgnoreCase("Yes")) {
	ignoreProxy = true;
    }

    trustAllVncCerts = false;
    str = readParameter("trustAllVncCerts", false);
    if (str != null && str.equalsIgnoreCase("Yes")) {
	trustAllVncCerts = true;
    }
    trustUrlVncCert = false;
    str = readParameter("trustUrlVncCert", false);
    if (str != null && str.equalsIgnoreCase("Yes")) {
	trustUrlVncCert = true;
    }
    debugCerts = false;
    str = readParameter("debugCerts", false);
    if (str != null && str.equalsIgnoreCase("Yes")) {
	debugCerts = true;
    }
    debugKeyboard = false;
    str = readParameter("debugKeyboard", false);
    if (str != null && str.equalsIgnoreCase("Yes")) {
	debugKeyboard = true;
    }
    mapF5_to_atsign = false;
    str = readParameter("mapF5_to_atsign", false);
    if (str != null && str.equalsIgnoreCase("Yes")) {
	mapF5_to_atsign = true;
    }
    forbid_Ctrl_Alt = false;
    str = readParameter("forbid_Ctrl_Alt", false);
    if (str != null && str.equalsIgnoreCase("Yes")) {
	forbid_Ctrl_Alt = true;
    }
  }

  public String readParameter(String name, boolean required) {
    if (inAnApplet) {
      String s = getParameter(name);
      if ((s == null) && required) {
	fatalError(name + " parameter not specified");
      }
      return s;
    }

    for (int i = 0; i < mainArgs.length; i += 2) {
      if (mainArgs[i].equalsIgnoreCase(name)) {
	try {
	  return mainArgs[i+1];
	} catch (Exception e) {
	  if (required) {
	    fatalError(name + " parameter not specified");
	  }
	  return null;
	}
      }
    }
    if (required) {
      fatalError(name + " parameter not specified");
    }
    return null;
  }

  int readIntParameter(String name, int defaultValue) {
    String str = readParameter(name, false);
    int result = defaultValue;
    if (str != null) {
      try {
	result = Integer.parseInt(str);
      } catch (NumberFormatException e) { }
    }
    return result;
  }

  //
  // moveFocusToDesktop() - move keyboard focus either to the
  // VncCanvas or to the AuthPanel.
  //

  void moveFocusToDesktop() {
    if (vncContainer != null) {
      if (vc != null && vncContainer.isAncestorOf(vc)) {
	vc.requestFocus();
      } else if (vncContainer.isAncestorOf(authenticator)) {
	authenticator.moveFocusToDefaultField();
      } else if (vncContainer.isAncestorOf(authenticatorUnixLogin)) {
	authenticatorUnixLogin.moveFocusToDefaultField();
      }
    }
  }

  //
  // disconnect() - close connection to server.
  //

  synchronized public void disconnect() {
    System.out.println("Disconnect");

    if (rfb != null && !rfb.closed())
      rfb.close();
    options.dispose();
    clipboard.dispose();
    if (rec != null)
      rec.dispose();

    if (inAnApplet) {
      showMessage("Disconnected");
    } else {
      System.exit(0);
    }
  }

  //
  // fatalError() - print out a fatal error message.
  // FIXME: Do we really need two versions of the fatalError() method?
  //

  synchronized public void fatalError(String str) {
    System.out.println(str);

    if (inAnApplet) {
      // vncContainer null, applet not inited,
      // can not present the error to the user.
      Thread.currentThread().stop();
    } else {
      System.exit(1);
    }
  }

  synchronized public void fatalError(String str, Exception e) {
 
    if (rfb != null && rfb.closed()) {
      // Not necessary to show error message if the error was caused
      // by I/O problems after the rfb.close() method call.
      System.out.println("RFB thread finished");
      return;
    }

    System.out.println(str);
    e.printStackTrace();

    if (rfb != null)
      rfb.close();

    if (inAnApplet) {
      showMessage(str);
    } else {
      System.exit(1);
    }
  }

  //
  // Show message text and optionally "Relogin" and "Close" buttons.
  //

  void showMessage(String msg) {
    vncContainer.removeAll();

    Label errLabel = new Label(msg, Label.CENTER);
    errLabel.setFont(new Font("Helvetica", Font.PLAIN, 12));

    if (offerRelogin) {

      Panel gridPanel = new Panel(new GridLayout(0, 1));
      Panel outerPanel = new Panel(new FlowLayout(FlowLayout.LEFT));
      outerPanel.add(gridPanel);
      vncContainer.setLayout(new FlowLayout(FlowLayout.LEFT, 30, 16));
      vncContainer.add(outerPanel);
      Panel textPanel = new Panel(new FlowLayout(FlowLayout.CENTER));
      textPanel.add(errLabel);
      gridPanel.add(textPanel);
      gridPanel.add(new ReloginPanel(this));

    } else {

      vncContainer.setLayout(new FlowLayout(FlowLayout.LEFT, 30, 30));
      vncContainer.add(errLabel);

    }

    if (inSeparateFrame) {
      vncFrame.pack();
    } else {
      validate();
    }
  }

  //
  // Stop the applet.
  // Main applet thread will terminate on first exception
  // after seeing that rfbThread has been set to null.
  //

  public void stop() {
    System.out.println("Stopping applet");
    rfbThread = null;
  }

  //
  // This method is called before the applet is destroyed.
  //

  public void destroy() {
    System.out.println("Destroying applet");

    vncContainer.removeAll();
    options.dispose();
    clipboard.dispose();
    if (rec != null)
      rec.dispose();
    if (rfb != null && !rfb.closed())
      rfb.close();
    if (inSeparateFrame)
      vncFrame.dispose();
  }

  //
  // Start/stop receiving mouse events.
  //

  public void enableInput(boolean enable) {
    vc.enableInput(enable);
  }

  //
  // Close application properly on window close event.
  //

  public void windowClosing(WindowEvent evt) {
    System.out.println("Closing window");
    if (rfb != null)
      disconnect();

    vncContainer.hide();

    if (!inAnApplet) {
      System.exit(0);
    }
  }

  //
  // Move the keyboard focus to the password field on window activation.
  //

  public void windowActivated(WindowEvent evt) {
    if (vncFrame.isAncestorOf(authenticator)) {
      authenticator.moveFocusToDefaultField();
    } else if (vncContainer.isAncestorOf(authenticatorUnixLogin)) {
      authenticatorUnixLogin.moveFocusToDefaultField();
    }
  }

  //
  // Ignore window events we're not interested in.
  //

  public void windowDeactivated (WindowEvent evt) {}
  public void windowOpened(WindowEvent evt) {}
  public void windowClosed(WindowEvent evt) {}
  public void windowIconified(WindowEvent evt) {}
  public void windowDeiconified(WindowEvent evt) {}
}
