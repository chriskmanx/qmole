/* XGraphicsDevice.java -- GraphicsDevice for X
   Copyright (C) 2006 Free Software Foundation, Inc.

This file is part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

Linking this library statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and
conditions of the GNU General Public License cover the whole
combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent
modules, and to copy and distribute the resulting executable under
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */

package gnu.java.awt.peer.x;

import gnu.classpath.SystemProperties;
import gnu.x11.Display;
import gnu.x11.EscherServerConnectionException;

import java.awt.GraphicsConfiguration;
import java.awt.GraphicsDevice;
import java.lang.reflect.Constructor;
import java.net.Socket;

/**
 * This class represents an X Display. The actual connection is established
 * lazily when it is first needed.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class XGraphicsDevice
  extends GraphicsDevice
{

  private XGraphicsConfiguration defaultConfiguration;

  /**
   * The X display associated with the XGraphicsDevice. This is established
   * when {@link #getDisplay} is first called.
   */
  private Display display;

  /**
   * The event pump for this X Display.
   */
  private XEventPump eventPump;

  /**
   * Creates a new XGraphicsDevice.
   */
  XGraphicsDevice(String displayVar)
  {
    this.display = initDisplay(parseDisplayVar(displayVar));
  }

  public int getType()
  {
    return TYPE_RASTER_SCREEN;
  }

  public String getIDstring()
  {
    // TODO: Implement this.
    throw new UnsupportedOperationException("Not yet implemented.");
  }

  public GraphicsConfiguration[] getConfigurations()
  {
    // TODO: Implement this.
    throw new UnsupportedOperationException("Not yet implemented.");
  }

  public GraphicsConfiguration getDefaultConfiguration()
  {
    if (defaultConfiguration == null)
      defaultConfiguration = new XGraphicsConfiguration(this);
    return defaultConfiguration;
  }

  /**
   * Returns the X Display associated with this XGraphicsDevice.
   *
   * @return the X Display associated with this XGraphicsDevice
   */
  Display getDisplay()
  {
    return display;
  }

  /**
   * Returns the {@link XEventPump} associated with this XGraphicsDevice
   * @return the X Event Pump
   */
  XEventPump getEventPump()
  {
    return eventPump;
  }

  /**
   * Tries to load the LocalSocket class and initiate a connection to the
   * local X server.
   */
  private Socket createLocalSocket(int displayNumber)
  {
    Socket socket = null;
    try
      {
        // TODO: Is this 100% ok?
        String sockPath = "/tmp/.X11-unix/X" + displayNumber;
        Class localSocketAddressClass =
          Class.forName("gnu.java.net.local.LocalSocketAddress");
        Constructor localSocketAddressConstr =
          localSocketAddressClass.getConstructor(new Class[]{ String.class });
        Object addr =
          localSocketAddressConstr.newInstance(new Object[]{ sockPath });
        Class localSocketClass =
          Class.forName("gnu.java.net.local.LocalSocket");
        Constructor localSocketConstructor =
          localSocketClass.getConstructor(new Class[]{localSocketAddressClass});
        Object localSocket =
          localSocketConstructor.newInstance(new Object[]{ addr });
        socket = (Socket) localSocket;
      }
    catch (Exception ex)
      {
        // Whatever goes wrong here, we return null.
      }
    return socket;
  }
  
  /**
   * Parses a DISPLAY environment variable, the variable value syntax is:
   * 
   * <pre>
   * hostname:displayNumber.screenNumber
   * </pre>
   * 
   * @param envVar the environment variable to parse
   * @return a String array of size 3 containing the hostname, displayNumber and
   *         screenNumber
   */
  private String[] parseDisplayVar(String envVar) {
    // Default Values
    String[] parsedVar = new String[] {"localhost", "0", "0"};
    
    if (envVar == null)
      return parsedVar;
  
    int i = envVar.indexOf(':');
  
    // Case 1: envVar = hostname
    if (i == -1) 
      {
        parsedVar[0] = envVar.equals("") ? "localhost" : envVar;
        return parsedVar;
      }
    
    String hostname = envVar.substring(0, i);
    parsedVar[0] = hostname.equals("") ? "localhost" : hostname;
    int j = envVar.indexOf('.', i);

    // Case 2: envVar = hostname:displayNumber
    if (j == -1) 
      {
        parsedVar[1] = envVar.substring(i + 1, envVar.length());
        return parsedVar;
      }
  
    // Case 3: envVar = hostname:displayNumber.screenNumber
    parsedVar[1] = envVar.substring(i + 1, j);
    parsedVar[2] = envVar.substring(j + 1, envVar.length());
    
    return parsedVar;
  }

  /**
   * Initialize the connection with the X11 Server through Escher
   * 
   * @param envVar a array of String containing the envVar parsed (hostname,
   *          displayNumber and screenNumber)
   * @return the X Display
   */
  private Display initDisplay(String[] envVar) {
    if (XToolkit.DEBUG)
      System.err.println("Connecting to = " + envVar[0] + ":" + envVar[1] + "." + envVar[2]);
    
    // Try to connect via unix domain sockets when host == localhost.
    if ((envVar[0].equals("localhost") || envVar[0].equals(""))
      && SystemProperties.getProperty("gnu.xawt.no_local_sockets") == null)
      {
        Socket socket = createLocalSocket(Integer.parseInt(envVar[1]));
        if (socket != null)
          {
            try
              {
                display = new Display(socket, "localhost",
                                      Integer.parseInt(envVar[1]),  // Display Number
                                      Integer.parseInt(envVar[2])); // Screen Number
              }
            catch (EscherServerConnectionException e)
              {
                throw new RuntimeException(e.getCause());
              }
          }
      }

    // The following happens when we are configured to use plain sockets,
    // when the connection is probably remote or when we couldn't load
    // the LocalSocket class stuff.
    if (display == null)
      {
        try
          {
            display = new Display(envVar[0],                    // Hostname
                                  Integer.parseInt(envVar[1]),  // Display Number
                                  Integer.parseInt(envVar[2])); // Screen Number
          }
        catch (EscherServerConnectionException e)
          {
            throw new RuntimeException(e.getCause());
          }
      }
    
    eventPump = new XEventPump(display);
    return display;
  }
}
