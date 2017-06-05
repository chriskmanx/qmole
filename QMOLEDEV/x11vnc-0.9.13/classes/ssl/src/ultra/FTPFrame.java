// Copyright (C) 2002-2005 Ultr@VNC Team.  All Rights Reserved.
// Copyright (C) 2004 Kenn Min Chong, John Witchel.  All Rights Reserved.
//
//This is free software; you can redistribute it and/or modify
//it under the terms of the GNU General Public License as published by
//the Free Software Foundation; either version 2 of the License, or
//(at your option) any later version.
//
//This software is distributed in the hope that it will be useful,
//but WITHOUT ANY WARRANTY; without even the implied warranty of
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//GNU General Public License for more details.
//
//You should have received a copy of the GNU General Public License
//along with this software; if not, write to the Free Software
//Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307,
//USA.
//


import javax.swing.JFrame;
import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.ArrayList;
import java.util.Vector;
import java.util.Date;
import javax.swing.*;

import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.*;

// begin runge/x11vnc
import java.util.Arrays;
// end runge/x11vnc


/*
 * Created on Feb 25, 2004
 *
 */

/**
 * @author John Witchel, Kenn Min Chong
 *
 */
public class FTPFrame extends JFrame implements ActionListener, MouseListener {
	VncViewer viewer;

	private javax.swing.JPanel jContentPane = null;
	private javax.swing.JPanel topPanel = null;
	private javax.swing.JPanel topPanelLocal = null;
	private javax.swing.JPanel topPanelRemote = null;
	private javax.swing.JPanel topPanelCenter = null;
	private javax.swing.JPanel statusPanel = null;
	private javax.swing.JPanel remotePanel = null;
	private javax.swing.JPanel localPanel = null;
	private javax.swing.JPanel buttonPanel = null;
	private javax.swing.JButton sendButton = null;
	private javax.swing.JButton receiveButton = null;
	private javax.swing.JButton deleteButton = null;
	private javax.swing.JButton newFolderButton = null;
	private javax.swing.JButton stopButton = null;
	private javax.swing.JButton closeButton = null;
	private javax.swing.JButton dummyButton = null;
	private javax.swing.JComboBox localDrivesComboBox = null;
	private javax.swing.JComboBox remoteDrivesComboBox = null;
	private javax.swing.JTextField localMachineLabel = null;
	private javax.swing.JTextField remoteMachineLabel = null;
	private javax.swing.JButton localTopButton = null;
	private javax.swing.JButton remoteTopButton = null;
	private javax.swing.JScrollPane localScrollPane = null;
	private javax.swing.JList localFileTable = null;
	private javax.swing.JScrollPane remoteScrollPane = null;
	private javax.swing.JList remoteFileTable = null;
	private javax.swing.JTextField remoteLocation = null;
	private javax.swing.JTextField localLocation = null;
	private javax.swing.JTextField localStatus = null;
	public javax.swing.JTextField remoteStatus = null;
	public javax.swing.JComboBox historyComboBox = null;
	public javax.swing.JProgressBar jProgressBar = null;
	public javax.swing.JTextField connectionStatus = null;
	public boolean updateDriveList;
	private Vector remoteList = null;
	private Vector remoteListInfo = null;
	private Vector localList = null;
	private Vector localListInfo = null;
	private File currentLocalDirectory = null;	// Holds the current local Directory
	private File currentRemoteDirectory = null;	// Holds the current remote Directory
	private File localSelection = null;		// Holds the currently selected local file  
	private String remoteSelection = null;	// Holds the currently selected remote file
	public String selectedTable = null;

// begin runge/x11vnc
	private javax.swing.JButton viewButton = null;
	private javax.swing.JButton refreshButton = null;
	public File saveLocalDirectory = null;
	public long saveLocalDirectoryTime = 0;
	public int saveLocalDirectoryCount = 0;
	public String saveRemoteDirectory = null;
	public long saveRemoteDirectoryTime = 0;
	public int saveRemoteDirectoryCount = 0;
	private boolean localCurrentIsDir = true;
	private int lastRemoteIndex = -1;
	private int lastLocalIndex = -1;
	private boolean doingShortcutDir = false;
	private boolean gotShortcutDir = false;
	private boolean ignore_events = false;
// end   runge/x11vnc
	
//	 sf@2004 - Separate directories and files for better lisibility
	private ArrayList DirsList;
	private ArrayList FilesList;	

	public static void main(String[] args) {
	}
	/**
	 * This is the default constructor
	 
	public FTPFrame() {
		super();
		initialize();
	}
	*/

	/**
	 * This is Kenn's Constructor
	 *
	 */
	FTPFrame(VncViewer v) {
		super("Ultr@VNC File Transfer");
		viewer = v;
		// this.setUndecorated(true); // sf@2004
		this.setResizable(false);  // sf@2004
		setSize(320, 240);
		
		// sf@2004
		DirsList = new ArrayList();
		FilesList = new ArrayList();
		
		initialize();
	}
	
	 /* Refreshing local and remote directory lists
	  * after an operation has been performed
	 */
	 void refreshLocalLocation()
	 {
	 	File f = new File(localLocation.getText());
	 	this.changeLocalDirectory(f);
	 }
	 
	 void refreshRemoteLocation()
	 {

//System.out.println("refreshRemoteLocation1");
		remoteList.clear();
		remoteListInfo.clear();
		remoteFileTable.setListData(remoteList);	
System.out.println("refreshRemoteLocation '" + remoteLocation.getText() + "'");	// runge/x11vnc
		viewer.rfb.readServerDirectory(remoteLocation.getText());
	 }
	 
// begin runge/x11vnc
	 public void setSavedLocations() {
		saveLocalDirectory = currentLocalDirectory;
		saveLocalDirectoryTime = System.currentTimeMillis();
		saveLocalDirectoryCount = 0;

		if (remoteLocation != null) {
			saveRemoteDirectory = remoteLocation.getText();
System.out.println("RemoteSave '" + saveRemoteDirectory + "'");
		}
		saveRemoteDirectoryTime = System.currentTimeMillis();
		saveRemoteDirectoryCount = 0;
	 }

	private File saveLocalHack(File dir) {
		saveLocalDirectoryCount++;
//System.out.println("L " + saveLocalDirectoryCount + " dt: " + (System.currentTimeMillis() - saveLocalDirectoryTime) + " - " + saveLocalDirectory);
		if (System.currentTimeMillis() > saveLocalDirectoryTime + 2000 || saveLocalDirectoryCount > 2) {
			saveLocalDirectory = null;
		}
		if (saveLocalDirectory != null) {
			currentLocalDirectory = saveLocalDirectory;
			localLocation.setText(saveLocalDirectory.toString());
			return saveLocalDirectory;
		} else {
			return dir;
		}
	}

	private String saveRemoteHack(String indrive) {
		saveRemoteDirectoryCount++;
//System.out.println("R " + saveRemoteDirectoryCount + " - " + saveRemoteDirectory);
		if (saveRemoteDirectory != null && saveRemoteDirectoryCount > 1) {
			saveRemoteDirectory = null;
		}
		if (saveRemoteDirectory != null) {
			if (! saveRemoteDirectory.equals("")) {
System.out.println("saveRemoteHack setText + refreshRemoteLocation '" + saveRemoteDirectory + "'");
				return saveRemoteDirectory;
			}
		}
		return indrive;
	}
// end runge/x11vnc


	/*
	 * Prints the list of drives on the remote directory and returns a String[].  
	 * str takes as string like A:fC:lD:lE:lF:lG:cH:c
	 * in the form Drive Letter:Drive Type where 
	 * f = floppy, l = local drive, c=CD-ROM, n = network
	 */
	String[] printDrives(String str) {
		System.out.println(str);
		updateDriveList = true;
		remoteDrivesComboBox.removeAllItems();
		int size = str.length();
		String driveType = null;
		String[] drive = new String[str.length() / 3];
		int idx = 0, C_drive = -1, O_drive = -1;

System.out.println("ComboBox: Str   '" + str + "'");

		// Loop through the string to create a String[]
		for (int i = 0; i < size; i = i + 3) {
			drive[i / 3] = str.substring(i, i + 2);
			driveType = str.substring(i + 2, i + 3);
			if (driveType.compareTo("f") == 0)
				drive[i / 3] += "\\ Floppy";
			if (driveType.compareTo("l") == 0) {
				drive[i / 3] += "\\ Local Disk";
				if (drive[i/3].substring(0,1).toUpperCase().equals("C")) {
					C_drive = idx;
				} else if (O_drive < 0) {
					O_drive = idx;
				}
			}
			if (driveType.compareTo("c") == 0)
				drive[i / 3] += "\\ CD-ROM";
			if (driveType.compareTo("n") == 0)
				drive[i / 3] += "\\ Network";

			remoteDrivesComboBox.addItem(drive[i / 3]);
System.out.println("ComboBox: Add " + idx + " '" + drive[i/3] + "'");
			idx++;
		}

		// runge
		if (viewer.ftpDropDown != null) {
			String[] dd = viewer.ftpDropDown.split("\\.");
			for (int i=0; i < dd.length; i++) {
				if (!dd[i].equals("")) {
					String s = dd[i];
					if (s.startsWith("TOP_")) {
						s = s.substring(4);
						remoteDrivesComboBox.insertItemAt(" [" + s + "]", 0);
					} else {
						remoteDrivesComboBox.addItem(" [" + s + "]");
					}
				}
			}
		} else {
			remoteDrivesComboBox.addItem(" [My Documents]");
			remoteDrivesComboBox.addItem(" [Desktop]");
			remoteDrivesComboBox.addItem(" [Home]");
		}

		//sf@ - Select Drive C:as default if possible
		boolean bFound = false;

		if (false) {
			for(int i = 0; i < remoteDrivesComboBox.getItemCount() ; i++) {
				if(remoteDrivesComboBox.getItemAt(i).toString().substring(0,1).toUpperCase().equals("C")) {
					remoteDrivesComboBox.setSelectedIndex(i);
					bFound = true;
				}
			}
		} else {
			if (C_drive >= 0) {
				remoteDrivesComboBox.setSelectedIndex(C_drive);
				bFound = true;
System.out.println("ComboBox: C_drive index: " + C_drive);
			} else if (O_drive >= 0) {
				remoteDrivesComboBox.setSelectedIndex(O_drive);
				bFound = true;
System.out.println("ComboBox: Other_drive index: " + O_drive);
			}
		}

		if (!bFound) remoteDrivesComboBox.setSelectedIndex(0);

		updateDriveList = false;
		return drive;
	}
	
	/*Disable buttons/lists while file transfer is in progress*/
	
	public void disableButtons()
	{
		closeButton.setEnabled(false);
		deleteButton.setEnabled(false);
		localTopButton.setEnabled(false);
		newFolderButton.setEnabled(false);
		stopButton.setVisible(true);
		stopButton.setEnabled(true);
		receiveButton.setEnabled(false);
		viewButton.setEnabled(false);	// runge/x11vnc
		refreshButton.setEnabled(false);
		remoteTopButton.setEnabled(false);
		sendButton.setEnabled(false);
		remoteFileTable.setEnabled(false);
		localFileTable.setEnabled(false);	
		localLocation.setEnabled(false);
		remoteLocation.setEnabled(false);	
		remoteDrivesComboBox.setEnabled(false);
		localDrivesComboBox.setEnabled(false);
		setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE); // sf@2004
		
	}
	/*Enable buttons/lists after file transfer is done*/
	
	public void enableButtons()
	{
		closeButton.setEnabled(true);
		deleteButton.setEnabled(true);
		localTopButton.setEnabled(true);
		newFolderButton.setEnabled(true);
		stopButton.setVisible(false);
		stopButton.setEnabled(false);
		receiveButton.setEnabled(true);
		viewButton.setEnabled(true);	// runge/x11vnc
		refreshButton.setEnabled(true);
		remoteTopButton.setEnabled(true);
		sendButton.setEnabled(true);
		remoteFileTable.setEnabled(true);
		localFileTable.setEnabled(true);
		localLocation.setEnabled(true);		
		remoteLocation.setEnabled(true);
		remoteDrivesComboBox.setEnabled(true);
		localDrivesComboBox.setEnabled(true);
		// setDefaultCloseOperation(JFrame.HIDE_ON_CLOSE); // sf@2004
	}

	/*
	 * Print Directory prints out all the contents of a directory
	 */
	void printDirectory(ArrayList a, ArrayList b) {

		for (int i = 0; i < a.size(); i++) {
			remoteList.addElement(a.get(i));
			remoteListInfo.addElement(b.get(i));
		}
		remoteFileTable.setListData(remoteList);
	}

	/**
	 * This method initializes this
	 * 
	 * @return void
	 */
	private void initialize() {
		ignore_events = true;
		this.setSize(794, 500);
		this.setContentPane(getJContentPane());
		ignore_events = false;
		updateDriveList = true;
	}
	/**
	 * This method initializes jContentPane.  This is the main content pane
	 * 
	 * @return javax.swing.JPanel
	 */
	private javax.swing.JPanel getJContentPane() {
		if (jContentPane == null) {
			jContentPane = new javax.swing.JPanel();
			jContentPane.setLayout(new java.awt.BorderLayout());
			jContentPane.add(getTopPanel(), java.awt.BorderLayout.NORTH);
			jContentPane.add(getStatusPanel(), java.awt.BorderLayout.SOUTH);
			jContentPane.add(getRemotePanel(), java.awt.BorderLayout.EAST);
			jContentPane.add(getLocalPanel(), java.awt.BorderLayout.WEST);
			jContentPane.add(getButtonPanel(), java.awt.BorderLayout.CENTER);

			KeyStroke stroke = KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0);
			AbstractAction escapeAction = new AbstractAction() {
				public void actionPerformed(ActionEvent actionEvent) {
					System.out.println("Escape Pressed");
					if (viewer.ftpOnly) {
						System.out.println("exiting...");
						System.exit(0);
					} else {
						doClose();
					}
				}
			};
			jContentPane.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).put(stroke, "escapeAction");
			jContentPane.getInputMap().put(stroke, "escapeAction");
			jContentPane.getActionMap().put("escapeAction", escapeAction);

			stroke = KeyStroke.getKeyStroke(KeyEvent.VK_R, InputEvent.CTRL_MASK);
			AbstractAction resetAction = new AbstractAction() {
				public void actionPerformed(ActionEvent actionEvent) {
					System.out.println("Ctrl-R Pressed");
					doReset();
				}
			};
			jContentPane.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).put(stroke, "resetAction");
			jContentPane.getInputMap().put(stroke, "resetAction");
			jContentPane.getActionMap().put("resetAction", resetAction);
		}
		return jContentPane;
	}
	/**
	 * This method initializes topPanel
	 * 
	 * @return javax.swing.JPanel
	 */
	private javax.swing.JPanel getTopPanelLocal() {
		if (topPanelLocal == null) {
			topPanelLocal = new javax.swing.JPanel();
			topPanelLocal.setLayout(new java.awt.BorderLayout());
			topPanelLocal.setPreferredSize(new java.awt.Dimension(325, 22));
			topPanelLocal.add(getLocalDrivesComboBox(), java.awt.BorderLayout.WEST);
			topPanelLocal.add(getLocalMachineLabel(), java.awt.BorderLayout.CENTER);
			topPanelLocal.add(getLocalTopButton(), java.awt.BorderLayout.EAST);
			topPanelLocal.setBackground(java.awt.Color.lightGray);
//System.out.println("getTopPanelLocal");
		}
		return topPanelLocal;
	}
	
	/**
	 * This method initializes topPanelRemote
	 * 
	 * @return javax.swing.JPanel
	 */
	private javax.swing.JPanel getTopPanelRemote() {
		if (topPanelRemote == null) {
			topPanelRemote = new javax.swing.JPanel();
			topPanelRemote.setLayout(new java.awt.BorderLayout());
			topPanelRemote.setPreferredSize(new java.awt.Dimension(325, 20));
			topPanelRemote.add(getRemoteDrivesComboBox(), java.awt.BorderLayout.WEST);
			topPanelRemote.add(getRemoteMachineLabel(), java.awt.BorderLayout.CENTER);
			topPanelRemote.add(getRemoteTopButton(), java.awt.BorderLayout.EAST);
			topPanelRemote.setBackground(java.awt.Color.lightGray);
//System.out.println("getTopPanelRemote");
		}
		return topPanelRemote;
	}

	/**
	 * This method initializes topPanelRemote
	 * 
	 * @return javax.swing.JPanel
	 */
	private javax.swing.JPanel getTopPanelCenter() {
		if (topPanelCenter == null) {
			topPanelCenter = new javax.swing.JPanel();
			topPanelCenter.add(getDummyButton(), null);
//System.out.println("getTopPanelCenter");
		}
		return topPanelCenter;
	}
	
	/**
	 * This method initializes topPanel
	 * 
	 * @return javax.swing.JPanel
	 */
	private javax.swing.JPanel getTopPanel() {
		if (topPanel == null) {
			topPanel = new javax.swing.JPanel();
			topPanel.setLayout(new java.awt.BorderLayout());
			//sf@2004 - We manage 2 top panels
			topPanel.add(getTopPanelLocal(), java.awt.BorderLayout.WEST);
			// topPanel.add(getTopPanelCenter(), java.awt.BorderLayout.CENTER);
			topPanel.add(getTopPanelRemote(), java.awt.BorderLayout.EAST);
						
			/*
			topPanel.add(getLocalDrivesComboBox(), null);
			topPanel.add(getLocalMachineLabel(), null);
			topPanel.add(getLocalTopButton(), null);
			topPanel.add(getRemoteDrivesComboBox(), null);
			topPanel.add(getRemoteMachineLabel(), null);
			topPanel.add(getRemoteTopButton(), null);
			topPanel.setBackground(java.awt.Color.lightGray);
			*/
//System.out.println("getTopPanel");
		}
		return topPanel;
	}

	/**
	 * This method initializes statusPanel
	 * 
	 * @return javax.swing.JPanel
	 */
	private javax.swing.JPanel getStatusPanel() {
		if (statusPanel == null) {
			statusPanel = new javax.swing.JPanel();
			statusPanel.setLayout(
				new javax.swing.BoxLayout(
					statusPanel,
					javax.swing.BoxLayout.Y_AXIS));
			statusPanel.add(getHistoryComboBox(), null);
			statusPanel.add(getJProgressBar(), null);
			statusPanel.add(getConnectionStatus(), null);
			statusPanel.setBackground(java.awt.Color.lightGray);
//System.out.println("getStatusPanel");
			
		}
		return statusPanel;
	}
	/**
	 * This method initializes remotePanel
	 * 
	 * @return javax.swing.JPanel
	 */
	private javax.swing.JPanel getRemotePanel() {
		if (remotePanel == null) {
			remotePanel = new javax.swing.JPanel();
			remotePanel.setLayout(
				new javax.swing.BoxLayout(
					remotePanel,
					javax.swing.BoxLayout.Y_AXIS));
			remotePanel.add(getRemoteLocation(), null);
			remotePanel.add(getRemoteScrollPane(), null);
			remotePanel.add(getRemoteStatus(), null);
			remotePanel.setBackground(java.awt.Color.lightGray);
//System.out.println("getRemotePanel");
		}
		return remotePanel;
	}
	/**
	 * This method initializes localPanel
	 * 
	 * @return javax.swing.JPanel
	 */
	private javax.swing.JPanel getLocalPanel() {
		if (localPanel == null) {
			localPanel = new javax.swing.JPanel();
			localPanel.setLayout(
				new javax.swing.BoxLayout(
					localPanel,
					javax.swing.BoxLayout.Y_AXIS));
			localPanel.add(getLocalLocation(), null);
			localPanel.add(getLocalScrollPane(), null);
			localPanel.add(getLocalStatus(), null);
			localPanel.setBackground(java.awt.Color.lightGray);
			localPanel.setComponentOrientation(
				java.awt.ComponentOrientation.UNKNOWN);
			localPanel.setName("localPanel");
//System.out.println("getLocalPanel");
		}
		return localPanel;
	}
	/**
	 * This method initializes buttonPanel
	 * 
	 * @return javax.swing.JPanel
	 */
	private javax.swing.JPanel getButtonPanel()
	{
		if (buttonPanel == null)
		{
			buttonPanel = new javax.swing.JPanel();
			buttonPanel.setLayout(null);
			buttonPanel.add(getReceiveButton(), null);
			buttonPanel.add(getRefreshButton(), null);	// runge/x11vnc
			buttonPanel.add(getViewButton(), null);	// runge/x11vnc
			buttonPanel.add(getNewFolderButton(), null);
			buttonPanel.add(getCloseButton(), null);
			buttonPanel.add(getDeleteButton(), null);
			buttonPanel.add(getSendButton(), null);
			buttonPanel.add(getStopButton(), null);
			buttonPanel.setBackground(java.awt.Color.lightGray);
//System.out.println("getButtonPanel");
		}
		return buttonPanel;
	}
	/**
	 * This method initializes sendButton
	 * 
	 * @return javax.swing.JButton
	 */
	private javax.swing.JButton getSendButton() {
		if (sendButton == null) {
			sendButton = new javax.swing.JButton();
			sendButton.setBounds(15, 30, 107, 25);	// runge/x11vnc
			sendButton.setText("Send >>");
			sendButton.setName("sendButton");
			sendButton.addActionListener(this);
//System.out.println("getSendButton");

		}
		return sendButton;
	}
	/**
	 * This method initializes receiveButton
	 * 
	 * @return javax.swing.JButton
	 */
	private javax.swing.JButton getReceiveButton() {
		if (receiveButton == null) {
			receiveButton = new javax.swing.JButton();
			receiveButton.setBounds(15, 60, 107, 25);	// runge/x11vnc
			receiveButton.setText("<< Receive");
			receiveButton.setName("receiveButton");
			receiveButton.addActionListener(this);
		}
		return receiveButton;
	}
	/**
	 * This method initializes deleteButton
	 * 
	 * @return javax.swing.JButton
	 */
	private javax.swing.JButton getDeleteButton() {
		if (deleteButton == null) {
			deleteButton = new javax.swing.JButton();
			deleteButton.setBounds(15, 110, 107, 25);	// runge/x11vnc
			deleteButton.setText("Delete File");
			deleteButton.setName("deleteButton");
			deleteButton.addActionListener(this);
		}
		return deleteButton;
	}
	/**
	 * This method initializes newFolderButton
	 * 
	 * @return javax.swing.JButton
	 */
	private javax.swing.JButton getNewFolderButton() {
		if (newFolderButton == null) {
			newFolderButton = new javax.swing.JButton();
			newFolderButton.setBounds(15, 140, 107, 25);	// runge/x11vnc
			newFolderButton.setText("New Folder");
			newFolderButton.setName("newFolderButton");
			newFolderButton.addActionListener(this);
		}
		return newFolderButton;
	}
	
// begin runge/x11vnc
	/**
	 * This method initializes refreshButton
	 * 
	 * @return javax.swing.JButton
	 */
	private javax.swing.JButton getRefreshButton() {
		if (refreshButton == null) {
			refreshButton = new javax.swing.JButton();
			refreshButton.setBounds(15, 170, 107, 25);
			refreshButton.setText("Refresh");
			refreshButton.setName("refreshButton");
			refreshButton.addActionListener(this);
		}
		return refreshButton;
	}
	/**
	 * This method initializes viewButton
	 * 
	 * @return javax.swing.JButton
	 */
	private javax.swing.JButton getViewButton() {
		if (viewButton == null) {
			viewButton = new javax.swing.JButton();
			viewButton.setBounds(15, 200, 107, 25);
			viewButton.setText("View File");
			viewButton.setName("viewButton");
			viewButton.addActionListener(this);
		}
		return viewButton;
	}
// end   runge/x11vnc

	/**
	 * This method initializes stopButton
	 * 
	 * @return javax.swing.JButton
	 */
	private javax.swing.JButton getStopButton()
	{
		if (stopButton == null)
		{
			stopButton = new javax.swing.JButton();
			stopButton.setBounds(15, 230, 107, 25);	// runge/x11vnc
			stopButton.setText("Stop");
			stopButton.setName("stopButton");
			stopButton.addActionListener(this);
			stopButton.setVisible(false);
		}
		return stopButton;
	}
	
	/**
	 * This method initializes closeButton
	 * 
	 * @return javax.swing.JButton
	 */
	private javax.swing.JButton getCloseButton() {
		if (closeButton == null) {
			closeButton = new javax.swing.JButton();
			closeButton.setBounds(15, 325, 107, 25);	// runge/x11vnc
			if (viewer.ftpOnly) {
				closeButton.setText("Quit");
			} else {
				closeButton.setText("Close");
			}
			closeButton.setName("closeButton");
			closeButton.addActionListener(this);
		}
		return closeButton;
	}
	
	/**
	 * This method initializes dummyButton
	 * 
	 * @return javax.swing.JButton
	 */
	private javax.swing.JButton getDummyButton() {
		if (dummyButton == null) {
			dummyButton = new javax.swing.JButton();
			dummyButton.setBounds(12, 206, 99, 25);
			dummyButton.setText("aaaaaaaaaaaaaaa");
			dummyButton.setName("DummyButton");
			dummyButton.setVisible(false);
		}
		return dummyButton;
	}
	
	/**
	 * This method initializes localDrivesComboBox
	 * 
	 * @return javax.swing.JComboBox
	 */
	private javax.swing.JComboBox getLocalDrivesComboBox() {
		updateDriveList = true;
		// Read in Drive letters from local disk
		File[] roots = File.listRoots();
		String[] localDisks = new String[roots.length];
		for (int i = 0; i < roots.length; i++) {
			localDisks[i] = roots[i].toString();
		}

		// Create the combo box
		if (localDrivesComboBox == null) {
			localDrivesComboBox = new javax.swing.JComboBox(localDisks);
			localDrivesComboBox.setName("LocalDisks");
			localDrivesComboBox.setFont(
				new java.awt.Font("Dialog", java.awt.Font.PLAIN, 10));

			//Select the second entry (e.g. C:\)
			// localDrivesComboBox.setSelectedIndex(1);
			localDrivesComboBox.addActionListener(this);
//System.out.println("getLocalDrivesComboBox");
		}
		updateDriveList = false;
		return localDrivesComboBox;
	}
	/**
	 * This method initializes remoteDrivesComboBox
	 * 
	 * @return javax.swing.JComboBox
	 */
	public javax.swing.JComboBox getRemoteDrivesComboBox() {
		if (remoteDrivesComboBox == null) {
			remoteDrivesComboBox = new javax.swing.JComboBox();
			remoteDrivesComboBox.setName("remoteDisks");
			remoteDrivesComboBox.setFont(
				new java.awt.Font("Dialog", java.awt.Font.PLAIN, 10));
			remoteDrivesComboBox.addActionListener(this);
//System.out.println("getRemoteDrivesComboBox");

		}
		return remoteDrivesComboBox;
	}
	/**
	 * This method initializes localMachineLabel
	 * 
	 * @return javax.swing.JTextField
	 */
	private javax.swing.JTextField getLocalMachineLabel() {
		if (localMachineLabel == null) {
			localMachineLabel = new javax.swing.JTextField();
			localMachineLabel.setAlignmentX(Component.CENTER_ALIGNMENT);
			// localMachineLabel.setPreferredSize(new java.awt.Dimension(150, 19));
			localMachineLabel.setBackground(java.awt.Color.lightGray);
			localMachineLabel.setText("             LOCAL MACHINE");
			localMachineLabel.setName("localLocation");
			localMachineLabel.setFont(
				new java.awt.Font("Dialog", java.awt.Font.BOLD, 11));
			localMachineLabel.setEditable(false);
//System.out.println("getLocalMachineLabel");
		}
		return localMachineLabel;
	}
	/**
	 * This method initializes remoteMachineLabel
	 * 
	 * @return javax.swing.JTextField
	 */
	private javax.swing.JTextField getRemoteMachineLabel() {
		if (remoteMachineLabel == null) {
			remoteMachineLabel = new javax.swing.JTextField();
			// remoteMachineLabel.setPreferredSize(new java.awt.Dimension(150, 19));
			remoteMachineLabel.setName("remoteLocation");
			remoteMachineLabel.setText("        REMOTE MACHINE");
			remoteMachineLabel.setBackground(java.awt.Color.lightGray);
			remoteMachineLabel.setFont(
				new java.awt.Font("Dialog", java.awt.Font.BOLD, 11));
			remoteMachineLabel.setEditable(false);
				
		}
		return remoteMachineLabel;
	}
	/**
	 * This method initializes localTopButton
	 * 
	 * @return javax.swing.JButton
	 */
	private javax.swing.JButton getLocalTopButton() {
		if (localTopButton == null) {
			localTopButton = new javax.swing.JButton();
			localTopButton.setText("Root (\\)");
			// localTopButton.setPreferredSize(new java.awt.Dimension(30, 19));
			localTopButton.setFont(
				new java.awt.Font("Dialog", java.awt.Font.BOLD, 10));
			localTopButton.addActionListener(this);
//System.out.println("getLocalTopButton");
		}
		return localTopButton;
	}
	/**
	 * This method initializes remoteTopButton
	 * 
	 * @return javax.swing.JButton
	 */
	private javax.swing.JButton getRemoteTopButton() {
		if (remoteTopButton == null) {
			remoteTopButton = new javax.swing.JButton();
			remoteTopButton.setText("Root (\\)");
			// remoteTopButton.setPreferredSize(new java.awt.Dimension(49, 25));
			remoteTopButton.setFont(
				new java.awt.Font("Dialog", java.awt.Font.BOLD, 10));
			remoteTopButton.addActionListener(this);
//System.out.println("getRemoteTopButton");
		}
		return remoteTopButton;
	}
	/**
	 * This method initializes localFileTable
	 * 
	 * @return javax.swing.JTable
	 */

	private javax.swing.JList getLocalFileTable() {
		if (localFileTable == null) {
			localList = new Vector(0);
			localListInfo = new Vector(0);
			localFileTable = new JList(localList);
			MouseMotionListener mlisten = new MouseMotionAdapter() {
				public void mouseMoved(MouseEvent e) {
					int index = localFileTable.locationToIndex(e.getPoint());
					if (index == lastLocalIndex) {
						return;
					} else if (index < 0) {
						return;
					}
					lastLocalIndex = index;
					connectionStatus.setText((String) localListInfo.get(index));
				}
			};
			localFileTable.addMouseListener(this);
			localFileTable.addMouseMotionListener(mlisten);
			localFileTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
//System.out.println("getLocalFileTable");
		}
		return localFileTable;
	}
	/**
	 * This method initializes localScrollPane
	 * 
	 * @return javax.swing.JScrollPane
	 */
	private javax.swing.JScrollPane getLocalScrollPane() {
		if (localScrollPane == null) {
			localScrollPane = new javax.swing.JScrollPane();
			localScrollPane.setViewportView(getLocalFileTable());
			localScrollPane.setPreferredSize(new java.awt.Dimension(325, 418));
			localScrollPane.setFont(
				new java.awt.Font("Dialog", java.awt.Font.PLAIN, 10));
			localScrollPane.setName("localFileList");
//System.out.println("getLocalScrollPane");
		}
		return localScrollPane;
	}
	/**
	 * This method initializes remoteFileTable
	 * 
	 * @return javax.swing.JTable
	 */
	private javax.swing.JList getRemoteFileTable() {
		if (remoteFileTable == null) {
			remoteList = new Vector(0);
			remoteListInfo = new Vector(0);
			remoteFileTable = new JList(remoteList);
			MouseMotionListener mlisten = new MouseMotionAdapter() {
				public void mouseMoved(MouseEvent e) {
					int index = remoteFileTable.locationToIndex(e.getPoint());
					if (index == lastRemoteIndex) {
						return;
					} else if (index < 0) {
						return;
					}
					lastRemoteIndex = index;
					connectionStatus.setText((String) remoteListInfo.get(index));
				}
			};
			remoteFileTable.addMouseListener(this);
			remoteFileTable.addMouseMotionListener(mlisten);
			remoteFileTable.setSelectedValue("C:\\", false);
			remoteFileTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
//System.out.println("getRemoteFileTable");
			
		}
		return remoteFileTable;
	}
	/**
	 * This method initializes remoteScrollPane
	 * 
	 * @return javax.swing.JScrollPane
	 */
	private javax.swing.JScrollPane getRemoteScrollPane() {
		if (remoteScrollPane == null) {
			remoteScrollPane = new javax.swing.JScrollPane();
			remoteScrollPane.setViewportView(getRemoteFileTable());
			remoteScrollPane.setPreferredSize(new java.awt.Dimension(325, 418));
//System.out.println("getRemoteScrollPane");
		}
		return remoteScrollPane;
	}
	/**
	 * This method initializes remoteLocation
	 * 
	 * @return javax.swing.JTextField
	 */
	private javax.swing.JTextField getRemoteLocation()
	{
		if (remoteLocation == null)
		{
			remoteLocation = new javax.swing.JTextField();
			remoteLocation.setText("");
			remoteLocation.setEditable(false); // sf@2004
			remoteLocation.setBackground(new Color(255,255,238));
			remoteLocation.setFont(
				new java.awt.Font("Dialog", java.awt.Font.PLAIN, 10));
//System.out.println("getRemoteLocation");
		}
		return remoteLocation;
	}
	/**
	 * This method initializes localLocation
	 * 
	 * @return javax.swing.JTextField
	 */
	private javax.swing.JTextField getLocalLocation() {
		if (localLocation == null) {
			localLocation = new javax.swing.JTextField();
			localLocation.setText("");
			localLocation.setEditable(false); // sf@2004
			localLocation.setBackground( new Color(255,255,238));
			localLocation.setFont(
				new java.awt.Font("Dialog", java.awt.Font.PLAIN, 10));
//System.out.println("getLocalLocation");
		}
		return localLocation;
	}
	/**
	 * This method initializes localStatus
	 * 
	 * @return javax.swing.JTextField
	 */
	private javax.swing.JTextField getLocalStatus() {
		if (localStatus == null) {
			localStatus = new javax.swing.JTextField();
			//		localStatus.setText("> Found 63 File(s) 7 Directorie(s)");
			localStatus.setBackground(java.awt.Color.lightGray);
			localStatus.setFont(
				new java.awt.Font("Dialog", java.awt.Font.PLAIN, 10));
			localStatus.setEditable(false);
//System.out.println("getLocalStatus");
		}
		return localStatus;
	}
	/**
	 * This method initializes remoteStatus
	 * 
	 * @return javax.swing.JTextField
	 */
	private javax.swing.JTextField getRemoteStatus() {
		if (remoteStatus == null) {
			remoteStatus = new javax.swing.JTextField();
			//		remoteStatus.setText("> Found 15 File(s) 2 Directorie(s)");
			remoteStatus.setBackground(java.awt.Color.lightGray);
			remoteStatus.setFont(
				new java.awt.Font("Dialog", java.awt.Font.PLAIN, 10));
			remoteStatus.setEditable(false);
//System.out.println("getRemoteStatus");
		}
		return remoteStatus;
	}
	/**
	 * This method initializes historyComboBox
	 * 
	 * @return javax.swing.JComboBox
	 */
	private javax.swing.JComboBox getHistoryComboBox() {
		if (historyComboBox == null) {
			historyComboBox = new javax.swing.JComboBox();
			historyComboBox.setFont(
				new java.awt.Font("Dialog", java.awt.Font.BOLD, 10));
			historyComboBox.insertItemAt(new String("Pulldown to view history;   Press Escape to Close/Quit;   Press Ctrl-R to Reset Panel."),0);
			historyComboBox.setSelectedIndex(0);
			historyComboBox.addActionListener(this);
//System.out.println("getHistoryComboBox");
		}
		return historyComboBox;
	}
	/**
	 * This method initializes jProgressBar
	 * 
	 * @return javax.swing.JProgressBar
	 */
	private javax.swing.JProgressBar getJProgressBar() {
		if (jProgressBar == null) {
			jProgressBar = new javax.swing.JProgressBar();
//System.out.println("getJProgressBar");
		}
		return jProgressBar;
	}
	/**
	 * This method initializes connectionStatus
	 * 
	 * @return javax.swing.JTextField
	 */
	private javax.swing.JTextField getConnectionStatus() {
		if (connectionStatus == null) {
			connectionStatus = new javax.swing.JTextField();
			connectionStatus.setText("Connected...");
			connectionStatus.setBackground(java.awt.Color.lightGray);
			connectionStatus.setFont(
				new java.awt.Font("Dialog", java.awt.Font.PLAIN, 10));
//System.out.println("getConnectionStatus");
		}
			connectionStatus.setEditable(false);
		return connectionStatus;
	}

	/**
	 * Implements Action listener.
	 */
	public void actionPerformed(ActionEvent evt) {
//		System.out.println(evt.getSource());

		if (ignore_events) {
			System.out.println("ignore_events: " + evt.getSource());
			return;
		}

		if (evt.getSource() == closeButton)
		{ // Close Button
			doClose();
		}
		else if (evt.getSource() == sendButton)
		{
			doSend();
		}
		else if (evt.getSource() == receiveButton)
		{
			doReceive();
		}
// begin runge/x11vnc
		else if (evt.getSource() == viewButton)
		{
			doView();
		}
// end   runge/x11vnc
		else if (evt.getSource() == localDrivesComboBox)
		{
			changeLocalDrive();
		}
		else if (evt.getSource() == remoteDrivesComboBox)
		{ 
//System.out.println("remoteDrivesComboBox");	// runge/x11vnc
			changeRemoteDrive();

			// are these really needed? changeRemoteDrive() does them at the end.
			if (false) {
				remoteList.clear();
				remoteListInfo.clear();
				remoteFileTable.setListData(remoteList);
			}
		}
		else if (evt.getSource() == localTopButton)
		{
			changeLocalDrive();
		}
		else if (evt.getSource() == remoteTopButton)
		{
//System.out.println("remoteTopButton");	// runge/x11vnc
		  	changeRemoteDrive();
		}
		else if(evt.getSource() == deleteButton)
		{
			doDelete();
		}
		else if(evt.getSource() == refreshButton)
		{
			doRefresh();
		}
		else if(evt.getSource()==newFolderButton)
		{
			doNewFolder();
		}
		else if (evt.getSource() == stopButton)
		{
			doStop();
		}

	}

	private void doNewFolder()
	{
		String name = JOptionPane.showInputDialog(jContentPane,"Enter new directory name", "Create New Directory", JOptionPane.QUESTION_MESSAGE);
		if(selectedTable.equals("remote"))
		{
			name = remoteLocation.getText()+name;
			viewer.rfb.createRemoteDirectory(name);
		}
		else
		{
			name = localLocation.getText()+name;
			File f = new File(name);
			f.mkdir();
			refreshLocalLocation();
			historyComboBox.insertItemAt(new String("Created Local Directory: " + name),0);
			historyComboBox.setSelectedIndex(0);
		}
	}
	public void doClose()
	{
		if (viewer.ftpOnly) {
			viewer.disconnect();
			return;
		}
		try {
			this.setVisible(false);
			viewer.rfb.writeFramebufferUpdateRequest(0, 0, viewer.rfb.framebufferWidth,
			    viewer.rfb.framebufferHeight, true);

			if (false) {
				this.dispose();
				jContentPane = null;
			}
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	private void unSwing() {
		jContentPane = null;
		topPanel = null;
		topPanelLocal = null;
		topPanelRemote = null;
		topPanelCenter = null;
		statusPanel = null;
		remotePanel = null;
		localPanel = null;
		buttonPanel = null;
		sendButton = null;
		receiveButton = null;
		deleteButton = null;
		newFolderButton = null;
		stopButton = null;
		closeButton = null;
		dummyButton = null;
		localDrivesComboBox = null;
		remoteDrivesComboBox = null;
		localMachineLabel = null;
		remoteMachineLabel = null;
		localTopButton = null;
		remoteTopButton = null;
		localScrollPane = null;
		localFileTable = null;
		remoteScrollPane = null;
		remoteFileTable = null;
		remoteLocation = null;
		localLocation = null;
		localStatus = null;
		remoteStatus = null;
		historyComboBox = null;
		jProgressBar = null;
		connectionStatus = null;
		viewButton = null;
		refreshButton = null;
	}

	public void doReset()
	{
		try {
			this.setVisible(false);
			this.dispose();
			jContentPane = null;
		        try {Thread.sleep(500);} catch (InterruptedException e) {}
			viewer.ftp_init();
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	public void doOpen()
	{
		try {
			this.setVisible(true);
			if (false) {
				this.initialize();
			}
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	private void doDelete()
	{
//		System.out.println("Delete Button Pressed");
		//Call this method to delete a file at server
		if(selectedTable.equals("remote"))
		{	
			Object selected = this.remoteFileTable.getSelectedValue();
			if (selected == null) {
				return;
			}
			String sFileName = ((String) selected);
			
//			 sf@2004 - Directory can't be deleted
			if (sFileName.substring(0, 2).equals(" [") && sFileName.substring((sFileName.length() - 1), sFileName.length()).equals("]"))
			{
				JOptionPane.showMessageDialog(jContentPane, (String)"Directory Deletion is not yet available in this version...", "FileTransfer Info", JOptionPane.INFORMATION_MESSAGE);
				return;
			}			
			
			// for (int i = 0; i < remoteList.contains(size(); i++) 
			// 	remoteFileTable.g(i));			
			// sf@2004 - Delete prompt
			if (remoteList.contains(sFileName))
			{
				int r = JOptionPane.showConfirmDialog(jContentPane, "Are you sure you want to delete the file \n< " + sFileName + " >\n on Remote Machine ?", "File Transfer Warning", JOptionPane.YES_NO_OPTION);
				if (r == JOptionPane.NO_OPTION)
					return;
			}
			
			String fileName = remoteLocation.getText()+ sFileName.substring(1);
			viewer.rfb.deleteRemoteFile(fileName);
		}
		else
		{
			Object selected = this.localFileTable.getSelectedValue();
			if (selected == null) {
				return;
			}
			String sFileName = ((String) selected);
			
//			 sf@2004 - Directory can't be deleted
			if (sFileName.substring(0, 2).equals(" [") && sFileName.substring((sFileName.length() - 1), sFileName.length()).equals("]"))
			{
				JOptionPane.showMessageDialog(jContentPane, (String)"Directory Deletion is not yet available in this version...", "FileTransfer Info", JOptionPane.INFORMATION_MESSAGE);
				return;
			}			
			// sf@2004 - Delete prompt
			if (localList.contains(sFileName))
			{
				int r = JOptionPane.showConfirmDialog(jContentPane, "Are you sure you want to delete the file \n< " + sFileName + " >\n on Local Machine ?", "File Transfer Warning", JOptionPane.YES_NO_OPTION);
				if (r == JOptionPane.NO_OPTION)
					return;
			}			
			String s = localLocation.getText() + sFileName.substring(1);
			File f = new File(s);
			f.delete();
			refreshLocalLocation();
			historyComboBox.insertItemAt(new String("Deleted On Local Disk: " + s),0);
			historyComboBox.setSelectedIndex(0);
		}
	}

	private void doReceive()
	{
//		System.out.println("Received Button Pressed");

		Object selected = this.remoteFileTable.getSelectedValue();
		if (selected == null) {
			return;
		}
		String sFileName = ((String) selected);
		
		// sf@2004 - Directory can't be transfered
		if (sFileName.substring(0, 2).equals(" [") && sFileName.substring((sFileName.length() - 1), sFileName.length()).equals("]"))
		{
			JOptionPane.showMessageDialog(jContentPane, (String)"Directory Transfer is not yet available in this version...", "FileTransfer Info", JOptionPane.INFORMATION_MESSAGE);
			return;
		}
		
		// sf@2004 - Overwrite prompt
		if (localList.contains(sFileName))
		{
			int r = JOptionPane.showConfirmDialog(jContentPane, "The file < " + sFileName + " >\n already exists on Local Machine\n Are you sure you want to overwrite it ?", "File Transfer Warning", JOptionPane.YES_NO_OPTION);
			if (r == JOptionPane.NO_OPTION)
				return;
		}
		
		//updateHistory("Downloaded " + localSelection.toString());
		String remoteFileName = this.remoteLocation.getText();
		remoteFileName+= ((String) this.remoteFileTable.getSelectedValue()).substring(1);
		
		String localDestinationPath = this.localLocation.getText()+((String)this.remoteFileTable.getSelectedValue()).substring(1);
		viewer.rfb.requestRemoteFile(remoteFileName,localDestinationPath);
	}

// begin runge/x11vnc
	private void doRefresh()
	{
		System.out.println("Refreshing Local and Remote.");
		refreshLocalLocation();
		refreshRemoteLocation();
	}

	private void doView()
	{
//		System.out.println("View Button Pressed");

		if (selectedTable == null) {
			return;
		}
		if (selectedTable.equals("remote")) {
			viewRemote();
		} else if (selectedTable.equals("local")) {
			viewLocal();
		}
	}

	private File doReceiveTmp()
	{

		if (remoteFileTable == null) {
			return null;
		}
		Object selected = this.remoteFileTable.getSelectedValue();
		if (selected == null) {
			return null;
		}
		String sFileName = ((String) selected);
		
		if (sFileName == null) {
			return null;
		}
		
		// sf@2004 - Directory can't be transfered
		if (sFileName.substring(0, 2).equals(" [") && sFileName.substring((sFileName.length() - 1), sFileName.length()).equals("]"))
		{
			return null;
		}
		
		File tmp = null;
		try {
			tmp = File.createTempFile("ULTRAFTP", ".txt");
		} catch (Exception e) {
			return null;
		}
		
		//updateHistory("Downloaded " + localSelection.toString());
		String remoteFileName = this.remoteLocation.getText();
		remoteFileName+= ((String) this.remoteFileTable.getSelectedValue()).substring(1);
		System.out.println("remoteFileName: " + remoteFileName);
if (false) {
		char[] b = remoteFileName.toCharArray();
		for (int n = 0; n < b.length; n++) {
			System.out.print(Integer.toHexString(b[n]) + " ");
		}
		System.out.println("");
		for (int n = 0; n < b.length; n++) {
			System.out.print(b[n]);
		}
		System.out.println("");
}
		
		String localDestinationPath = tmp.getAbsolutePath();
		viewer.rfb.requestRemoteFile(remoteFileName,localDestinationPath);
		System.out.println("ReceiveTmp: " + localDestinationPath);
		return tmp;
	}
// end   runge/x11vnc

	private void doSend()
	{
//		System.out.println("Send Button Pressed");

		Object selected = this.localFileTable.getSelectedValue();
		if (selected == null) {
			return;
		}
		String sFileName = ((String) selected);
		
		// sf@2004 - Directory can't be transfered
		if (sFileName.substring(0, 2).equals(" [") && sFileName.substring((sFileName.length() - 1), sFileName.length()).equals("]"))
		{
			JOptionPane.showMessageDialog(jContentPane, (String)"Directory Transfer is not yet available in this version...", "FileTransfer Info", JOptionPane.INFORMATION_MESSAGE);
			return;
		}
		
		// sf@2004 - Overwrite prompt
		if (remoteList.contains(sFileName))
		{
			int r = JOptionPane.showConfirmDialog(jContentPane, "The file < " + sFileName + " >\n already exists on Remote Machine\n Are you sure you want to overwrite it ?", "File Transfer Warning", JOptionPane.YES_NO_OPTION);
			if (r == JOptionPane.NO_OPTION)
				return;
		}
		//updateHistory("Uploaded " + localSelection.toString());
		String source = this.localLocation.getText();
		source += ((String) this.localFileTable.getSelectedValue()).substring(1);
		
		String destinationPath = this.remoteLocation.getText();
		
		viewer.rfb.offerLocalFile(source,destinationPath); 
	}
	
	//
	// sf@2004 - The user stops the current file transfer
	// 
	private void doStop()
	{
		System.out.println("** Current Transfer Aborted **");
		viewer.rfb.fAbort = true;
	}
	/**
	 * Update History: This method updates the history pulldown menu with the message string
	 *
	 */
	private void updateHistory(String message)
	{
		System.out.println("History: " + message);
		historyComboBox.insertItemAt(new String(message), 0);
	}

	public void receivedRemoteDirectoryName(String str) {
		if (doingShortcutDir) {
			if (str.length() > 1) {
				remoteLocation.setText(str);
			}
		}
	}
	
	/**
	 * This method updates the file table to the current selection of the remoteComboBox
	 *
	 */
	public void changeRemoteDrive()
	{
		remoteSelection = null;
	
		if (!updateDriveList) {
//System.out.println("changeRemoteDrive-A " + drive);	// begin runge/x11vnc
			Object selected = remoteDrivesComboBox.getSelectedItem();
			if (selected != null)  {
				String instr =	selected.toString();
				if (instr != null) {
System.out.println("changeRemoteDrive: instr='" + instr + "'");
					String drive =	instr.substring(0,1)+ ":\\";
					if (instr.startsWith(" [")) {
						int idx = instr.lastIndexOf(']'); 
						if (idx > 2) {
							drive = instr.substring(2, idx);
						} else {
							drive = instr.substring(2);
						}
						if (drive.equals("Home")) {
							drive = "";
						}
						drive += "\\";
						doingShortcutDir = true;
					} else {
						doingShortcutDir = false;
						drive = saveRemoteHack(drive);
					}
					gotShortcutDir = false;
					viewer.rfb.readServerDirectory(drive);
					if (!gotShortcutDir) {
						remoteLocation.setText(drive);
					}
				} else {
System.out.println("changeRemoteDrive: instr null");
				}
			} else {
System.out.println("changeRemoteDrive: selection null");
			}
//System.out.println("changeRemoteDrive-B " + drive);	// end runge/x11vnc
		}
		remoteList.clear();
		remoteListInfo.clear();
		remoteFileTable.setListData(remoteList);
	}
	/**
	 * changeLocalDrive updates the file table
	 * to the current selection of the localComboBox
	 */
	private void changeLocalDrive()
	{
		File currentDrive = new File(localDrivesComboBox.getSelectedItem().toString());
System.out.println("changeLocalDrive " + currentDrive.toString());	// runge/x11vnc
		if(currentDrive.canRead())
		{
			localSelection = null;
			localStatus.setText("");
			changeLocalDirectory(currentDrive);
		}
		else
		{
			localList.clear();
			localListInfo.clear();
			localStatus.setText("WARNING: Drive " + localDrivesComboBox.getSelectedItem().toString());
			connectionStatus.setText(" > WARNING - Local Drive unavailable (possibly restricted access or media not present)");
		}

	}
	/**
	 * Determines which FileTable was double-clicked and updates the table
	 */
	public void mouseClicked(MouseEvent e)
	{
		
		if(e.getClickCount() == 1)
		{								// Single clicked
			if (e.getSource() == localFileTable )
			{  			// on local file table 
				updateLocalFileTableSelection();
			}
			else if (e.getSource() == remoteFileTable)
			{
				updateRemoteFileTableSelection();						// on a remote file table
			}
		}
		else if (e.getClickCount() == 2)
		{						// Mouse Double clicked
			if (e.getSource() == localFileTable)
			{				// Clicked on local file
				updateLocalFileTable();
			}
			else if (e.getSource() == remoteFileTable)
			{		// Clicked on remote file
				updateRemoteFileTable();
			}
		}
	}
	/*
	 * Updates the globally accessible remote file selection if a file is single clicked in the RemoteFileTable
	 *
	 */
	private void updateRemoteFileTableSelection() {
		selectedTable = "remote";
		localFileTable.setBackground(new Color(238, 238, 238));
		remoteFileTable.setBackground(new Color(255, 255, 255));
		Object selected = remoteFileTable.getSelectedValue();
		if (selected == null) {
			return;
		}
		String selstr = selected.toString();
		if (selstr == null) {
			return;
		}
		String name = selstr.substring(1);
		if( !name.substring(0, 2).equals(" ["))	
			remoteSelection = remoteLocation.getText() + name.substring(0, name.length());

	}

	/*
	 * Updates the globally accessible local file selection 
	 * if a file is single clicked in the LocalFileTable 
	 *
	 */
	private void updateLocalFileTableSelection() {
		selectedTable="local";
		remoteFileTable.setBackground(new Color(238, 238, 238));
		localFileTable.setBackground(new Color(255, 255, 255));
		File currentSelection = new File(currentLocalDirectory, getTrimmedSelection());
		
// begin runge/x11vnc
		// localSelection = currentSelection.getAbsoluteFile();
		if(currentSelection.isFile()) {
			localSelection = currentSelection.getAbsoluteFile();
			localCurrentIsDir = false;
		} else {
			localCurrentIsDir = true;
		}
// end   runge/x11vnc

	}

// begin runge/x11vnc
	private void viewRemote() {
		File tmp = doReceiveTmp();
		if (tmp == null) {
			return;
		}
		TextViewer tv = new TextViewer("Remote: " + remoteSelection, tmp, true);
	}
	private void viewLocal() {
		if (localSelection == null) {
			return;
		}
		if (localCurrentIsDir) {
			return;
		}
		File loc = new File(localSelection.toString());
		TextViewer tv = new TextViewer("Local: " + localSelection.toString(), loc, false);
	}
// end runge/x11vnc

	/**
	 * Updates the Remote File Table based on selection.  Called from mouseClicked handler
	 */
	public void updateRemoteFileTable() {
		String name = null;
		String action = null;
		String drive = null;
		Object selected = remoteFileTable.getSelectedValue();
		if (selected == null) {
			return;
		}
		String sname = selected.toString();
		if (sname == null) {
			return;
		}
		name = sname.substring(1);

		if (name.equals("[..]"))
		{
			action = "up";
			remoteSelection = null;
			drive = remoteLocation.getText().substring(0, remoteLocation.getText().length() - 1);
			// JOptionPane.showMessageDialog(jContentPane, (String)drive, "FileTransfer DEBUG", JOptionPane.INFORMATION_MESSAGE);
			int index = drive.lastIndexOf("\\");
			drive = drive.substring(0, index + 1);

			remoteLocation.setText(drive);
			viewer.rfb.readServerDirectory(drive);
			remoteList.clear();
			remoteListInfo.clear();
			remoteFileTable.setListData(remoteList);	
		}
		else if (!name.substring(0, 2).equals(" [") && !name.substring((name.length() - 1), name.length()).equals("]"))
		{
			action = "file";
			// Set the global remoteSelection field (used for get/put buttons)
			remoteSelection = remoteLocation.getText() + name.substring(0, name.length());
			drive = remoteLocation.getText();
			// ??
			viewRemote();	// runge/x11vnc
		}
		else
		{ 
			action = "down";
			remoteSelection = null;
			name = name.substring(1, name.length() - 1);
			drive = remoteLocation.getText() + name + "\\";
			remoteLocation.setText(drive);
			viewer.rfb.readServerDirectory(drive);
			remoteList.clear();
			remoteListInfo.clear();
			remoteFileTable.setListData(remoteList);	
		}	
		//remoteLocation.setText(drive);	
	}

	/**
	 * Updates the Local File Table based on selection. Called from MouseClicked handler
	 */

	private void updateLocalFileTable()
	{
		localStatus.setText("");
		File currentSelection = new File(currentLocalDirectory , getTrimmedSelection());		// Selection

		if (getTrimmedSelection().equals(".."))
		{ // The [..] selected
			localSelection = null;	// No selection since directory changed
			currentSelection = currentLocalDirectory.getParentFile();
			if(currentSelection != null)
			{
				changeLocalDirectory(currentSelection);
			}
			else
			{
				localStatus.setText("You are at the root !"); 
			}
		}
		else if (currentSelection.isFile())
		{
			localSelection = currentSelection.getAbsoluteFile();
			viewLocal();	// runge/x11vnc
		}
		else if (currentSelection.isDirectory())
		{
			localSelection = null;	// No selection since directory changed
			changeLocalDirectory(currentSelection);
		}
	}

	/*
	 * Trims off the [] of a directory entry if it exists, else ignores it
	 * 
	 */
	private String getTrimmedSelection(){
		String currentSelection = "";
		Object selected = localFileTable.getSelectedValue();
		if (selected == null) {
			return currentSelection;
		}
		String selstr = selected.toString();
		if (selstr == null) {
			return currentSelection;
		}
		currentSelection = selstr.substring(1);
		if(currentSelection.substring(0,1).equals("[") &&
			currentSelection.substring(currentSelection.length()-1,currentSelection.length()).equals("]")){
			return currentSelection.substring(1,currentSelection.length()-1);
		} else {
			return currentSelection;
		}
	}

	/*
	 *  Reads the localDriveComboBox and returns the first readable drive for populating
	 *  the file table on load, so it's not looking at the A:\ drive when it opens. 
	 */
	 public File getFirstReadableLocalDrive(){
		File currentDrive;
		// sf@ - Select C: as default first readable drive
		for(int i = 0; i < localDrivesComboBox.getItemCount() ; i++)
		{
			currentDrive = new File(localDrivesComboBox.getItemAt(i).toString());
			if(localDrivesComboBox.getItemAt(i).toString().substring(0,1).toUpperCase().equals("C") && currentDrive.canRead())
			{
				localDrivesComboBox.setSelectedIndex(i);
				return currentDrive;
			}
		}
		// if C: not available, take the first readable drive, this time.
		for(int i = 0; i < localDrivesComboBox.getItemCount() ; i++)
		{
			currentDrive = new File(localDrivesComboBox.getItemAt(i).toString());
			if(currentDrive.canRead())
			{
				localDrivesComboBox.setSelectedIndex(i);
				return currentDrive;
			}
		}
		
		localStatus.setText("ERROR!: No Local Drives are Readable"); 
	 	return null;
	}
	
	String timeStr(long t) {
		Date date = new Date(t);
		return date.toString();
	}
	String dotPast(double f, int n) {
		String fs = "" + f;
		int i = fs.lastIndexOf(".") + n;
		if (i >= 0) {
			int len = fs.length();
			if (i >= len) {
				i = len-1;
			}
			fs = fs.substring(0, i);
		}
		return fs;
	}
	String sizeStr(int s) {
		if (s < 0) {
			return s + "? B";
		} else if (s < 1024) {
			return s + " B";
		} else if (s < 1024 * 1024) {
			double k = s / 1024.0;
			String ks = dotPast(k, 3);
			
			return s + " (" + ks + " KB)";
		} else {
			double m = s / (1024.0*1024.0);
			String ms = dotPast(m, 3);
			return s + " (" + ms + " MB)";
		}
	}

	int max_char(String text) {
		int maxc = 0;
		char chars[] = text.toCharArray();
		for (int n = 0; n < chars.length; n++) {
			if ((int) chars[n] > maxc) {
				maxc = (int) chars[n];
			}
		}
		return maxc;
	}

	/*
	 * Navigates the local file structure up or down one directory
	 */
	public void changeLocalDirectory(File dir)
	{
			dir = saveLocalHack(dir);	// runge/x11vnc

			if (dir == null) {
				connectionStatus.setText("Error changing local directory.");
				historyComboBox.insertItemAt(new String("> Error changing local directory."), 0);
				historyComboBox.setSelectedIndex(0);
				return;
			}

			File allFiles[] = dir.listFiles();	// Reads files
			String[] contents = dir.list();

			if (contents == null || allFiles == null) {
				connectionStatus.setText("Error changing local directory.");
				historyComboBox.insertItemAt(new String("> Error changing local directory."), 0);
				historyComboBox.setSelectedIndex(0);
				return;
			}

			currentLocalDirectory = dir;	// Updates Global
// begin runge/x11vnc
System.out.println("changeLocalDirectory: " + dir.toString());
			if (contents != null) {
				java.util.Arrays.sort(contents, String.CASE_INSENSITIVE_ORDER);
				for (int i = 0; i < contents.length; i++) {
					allFiles[i] = new File(dir, contents[i]);
				}
			} else {
				return;
			}
// end runge/x11vnc

			localList.clear();
			localListInfo.clear();
			localList.addElement(" [..]");
			localListInfo.addElement(" [..]");

			ArrayList DirInfo = new ArrayList();
			ArrayList FilInfo = new ArrayList();

			Charset charset = Charset.forName("ISO-8859-1");
			CharsetDecoder decoder = charset.newDecoder();
			CharsetEncoder encoder = charset.newEncoder();
			
			// Populate the Lists
			for (int i = 0; i < contents.length; i++)
			{
				String f1 = contents[i];

if (false) {
	
System.out.println("max_char: " + max_char(f1) + " "  + f1);
				if (max_char(f1) > 255) {
					try {
System.out.println("bbuf1");
						ByteBuffer bbuf = encoder.encode(CharBuffer.wrap(f1.toCharArray()));
System.out.println("bbuf2");
						CharBuffer cbuf = decoder.decode(bbuf);
System.out.println("bbuf3");
						f1 = cbuf.toString(); 
System.out.println("did bbuf: " + f1);
					} catch (Exception e) {
						;
					}
				}
}
				
				String f2 = f1;
				if (f2.length() < 24) {
					for (int ik = f2.length(); ik < 24; ik++) {
						f2 = f2 + " ";
					}
				}
				String s = f2 + "    \tLastmod: " + timeStr(allFiles[i].lastModified()) + "    \t\tSize: " + sizeStr((int) allFiles[i].length()); 
				if (allFiles[i].isDirectory()) {
					// localList.addElement("[" + contents[i] + "]");
					DirsList.add(" [" + f1 + "]"); // sf@2004
					DirInfo.add(s);
				} else {
					// localList.addElement(contents[i]);
					FilesList.add(" " + f1); // sf@2004
					FilInfo.add(s);
				}
			}
			// sf@2004
			for (int i = 0; i < DirsList.size(); i++) {
				localList.addElement(DirsList.get(i));
				localListInfo.addElement(DirInfo.get(i));
			}
			for (int i = 0; i < FilesList.size(); i++) {
				localList.addElement(FilesList.get(i));
				localListInfo.addElement(FilInfo.get(i));
			}
			
			FilesList.clear();
			DirsList.clear();
			
			localFileTable.setListData(localList);
			if(dir.toString().charAt(dir.toString().length()-1)==(File.separatorChar))
			{
				localLocation.setText(dir.toString());
			}
			else
			{
				localLocation.setText(dir.toString()+File.separator);	// Display updated location above file table
			}
			localStatus.setText("Total Files / Folders: " + (localList.size()-1));
	}
	public void mouseEntered(MouseEvent e) {
	}
	public void mouseExited(MouseEvent e) {
	}
	public void mousePressed(MouseEvent e) {
	}
	public void mouseReleased(MouseEvent e) {
	}

} //  @jve:visual-info  decl-index=0 visual-constraint="10,10"

// begin runge/x11vnc
class TextViewer extends JFrame implements ActionListener {

	JTextArea textArea = new JTextArea(35, 80);
	File file = null;
	JButton refreshButton;
	JButton dismissButton;
	Timer tim = null;
	int rcnt = 0;
	int tms = 250;
	boolean delete_it = false;
	TextViewer me;

	public TextViewer(String s, File f, boolean d) {

		delete_it = d;
		file = f;
		me = this;

		JScrollPane scrollPane = new JScrollPane(textArea,
		    JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
		    JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);

		textArea.setEditable(false);
		textArea.setFont(new Font("Monospaced", Font.PLAIN, 12));

		KeyStroke stroke = KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, InputEvent.SHIFT_MASK);
		AbstractAction escapeAction = new AbstractAction() {
			public void actionPerformed(ActionEvent actionEvent) {
				cleanse();
				me.dispose();
			}
		};
		textArea.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).put(stroke, "escapeAction");
		textArea.getInputMap().put(stroke, "escapeAction");
		textArea.getActionMap().put("escapeAction", escapeAction);

		refreshButton = new JButton();
		refreshButton.setText("Reload");
		refreshButton.setName("refreshButton");
		refreshButton.addActionListener(this);

		dismissButton = new JButton();
		dismissButton.setText("Dismiss");
		dismissButton.setName("dismissButton");
		dismissButton.addActionListener(this);

		JPanel buttons = new JPanel();
		buttons.setLayout(new BorderLayout());
		buttons.add(refreshButton, BorderLayout.WEST);
		buttons.add(dismissButton, BorderLayout.EAST);

		JPanel content = new JPanel();
		content.setLayout(new BorderLayout());
		content.add(scrollPane, BorderLayout.CENTER);
		content.add(buttons, BorderLayout.SOUTH);

		ActionListener tsk = new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				// System.out.println("tsk");
				refresh();
			}
		};
		tim = new Timer(tms, tsk);
		tim.start();

		this.setContentPane(content);
		this.setTitle("TextViewer - " + s);
		this.pack();
		this.setVisible(true);
	}

	private void refresh() {

		rcnt++;
		if (rcnt * tms > 3000 && tim != null) {
			tim.stop();
			tim = null;
		}
		BufferedReader input = null;
		StringBuffer contents = new StringBuffer();
		try {
			if (input == null) {
				input = new BufferedReader(new FileReader(file));
			}
			String line = null;
			int i = 0;
			while (( line = input.readLine()) != null) {
				if (i == 0) {
					// System.out.println("read");
				}
				i++;
				contents.append(line);
				contents.append(System.getProperty("line.separator"));
			}
		} catch (Exception e) {
			;
		} finally {
			try {
				if (input != null) {
					input.close();
					input = null;
				}
			} catch (Exception e) {
				;
			}
		}

		textArea.setText(contents.toString());
		textArea.setCaretPosition(0);
	}

	public void actionPerformed(ActionEvent evt) {

		if (evt.getSource() == refreshButton) {
			refresh();
		}
		if (evt.getSource() == dismissButton) {
			cleanse();
			this.dispose();
		}
	}

	private void cleanse() {
		if (delete_it && file != null) {
			try {
				file.delete();
				file = null;
			} catch (Exception e) {
				;
			}
		}
	}

	protected void finalize() throws Throwable {
		try {
			cleanse();
		} finally {
			super.finalize();
		}
	}
}
// end runge/x11vnc
