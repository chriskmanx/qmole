package gnu.x11;

import gnu.x11.event.ButtonPress;
import gnu.x11.event.ButtonRelease;
import gnu.x11.event.CirculateNotify;
import gnu.x11.event.CirculateRequest;
import gnu.x11.event.ClientMessage;
import gnu.x11.event.ColormapNotify;
import gnu.x11.event.ConfigureNotify;
import gnu.x11.event.ConfigureRequest;
import gnu.x11.event.CreateNotify;
import gnu.x11.event.DestroyNotify;
import gnu.x11.event.EnterNotify;
import gnu.x11.event.Event;
import gnu.x11.event.Expose;
import gnu.x11.event.FocusIn;
import gnu.x11.event.FocusOut;
import gnu.x11.event.GraphicsExpose;
import gnu.x11.event.GravityNotify;
import gnu.x11.event.KeyPress;
import gnu.x11.event.KeyRelease;
import gnu.x11.event.KeymapNotify;
import gnu.x11.event.LeaveNotify;
import gnu.x11.event.MapNotify;
import gnu.x11.event.MapRequest;
import gnu.x11.event.MappingNotify;
import gnu.x11.event.MotionNotify;
import gnu.x11.event.NoExposure;
import gnu.x11.event.PropertyNotify;
import gnu.x11.event.ReparentNotify;
import gnu.x11.event.ResizeRequest;
import gnu.x11.event.SelectionClear;
import gnu.x11.event.SelectionNotify;
import gnu.x11.event.SelectionRequest;
import gnu.x11.event.UnmapNotify;
import gnu.x11.event.VisibilityNotify;
import gnu.x11.extension.EventFactory;

import java.io.EOFException;
import java.io.FilterInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.LinkedList;
import java.util.List;

/**
 * Reads response data from the X server.
 * 
 * @author Roman Kennke (roman@kennke.org)
 */
public class ResponseInputStream extends FilterInputStream {

  /**
   * The display to which this input stream is associated.
   */
  private Display display;

  /**
   * Events that have already been read from the stream but not fetched.
   */
  private LinkedList events = new LinkedList ();

  /**
   * Creates a new ResponseInputStream.
   *
   * @param source the stream to read from
   */
  ResponseInputStream (InputStream source, Display d) {
    super (source);
    display = d;
  }

  /**
   * Skips n bytes in the stream.
   *
   * @param n the number of bytes to skip
   *
   * @return the actual number of bytes skipped
   */
  public long skip (long n) {
    assert Thread.holdsLock (this);

    long s = 0;
    try {
      while (s < n)
        s += super.skip (n - s);
    } catch (Exception ex) {
      handle_exception (ex);
    }
    return s;
  }

  public int read_int8 () {
    assert Thread.holdsLock (this);

    int v = -1;
    try {
      v = read ();
    } catch (IOException ex) {
      handle_exception (ex);
    }
    return v;
  }

  /**
   * Reads an INT16 value from the stream.
   *
   * @return the value
   */
  public int read_int16 () {
    assert Thread.holdsLock (this);

    int v = -1;
    try {
      v = (read () << 8) | read ();
    } catch (IOException ex) {
      handle_exception (ex);
    }
    return v;
  }

  /**
   * Reads an INT32 value from the stream.
   *
   * @return the value
   */
  public int read_int32 () {
    assert Thread.holdsLock (this);

    int v = -1;
    try {
      v = (read () << 24) | (read () << 16) | (read () << 8) | read ();
    } catch (IOException ex) {
      handle_exception (ex);
    }
    return v;
  }

  /**
   * Reads an INT32 value from the stream.
   *
   * @return the value
   */
  public long read_int64 () {
    assert Thread.holdsLock (this);

    long v = -1;
    try {
      v = (read () << 56) | (read () << 48) | (read () << 40) | (read () << 32)
          |(read () << 24) | (read () << 16) | (read () << 8) | read ();
    } catch (IOException ex) {
      handle_exception (ex);
    }
    return v;
  }

  public float read_float32 () {
    int bits = read_int32 ();
    float v = Float.intBitsToFloat (bits);
    return v;
  }

  public double read_float64 () {
    long bits = read_int64 ();
    double v = Double.longBitsToDouble (bits);
    return v;
  }

  public String read_string8 (int len) {
    assert Thread.holdsLock (this);

    byte [] buf = new byte [len];
    read_data (buf);
    String s = new String (buf);
    return s;
  }

  public void pad (int n) {
    assert Thread.holdsLock (this);

    int pad = n % 4;
    if (pad > 0)
      pad = 4 - pad;
    skip (pad);
  }

  public boolean read_bool () {
    assert Thread.holdsLock (this);

    boolean v = false;
    try {
      v = read () != 0;
    } catch (IOException ex) {
      handle_exception (ex);
    }
    return v;
  }

  /**
   * Reads an (unsigned) byte value from the underlying stream.
   *
   * @return the byte value
   */
  public int read_byte () {
    assert Thread.holdsLock (this);

    int v = -1;
    try {
      v = read ();
    } catch (IOException ex) {
      handle_exception (ex);
    }
    return v & 0xff;
  }

  public void read_data (byte [] buf, int offset, int len) {
    assert Thread.holdsLock (this);

    try {
      while (len > 0) {
        int numread = in.read (buf, offset, len);
        if (numread < 0)
          throw new EOFException ();
        len -= numread;
        offset += numread;
      }
    } catch (IOException ex) {
      handle_exception (ex);
    }
  }

  public void read_data (byte [] buf) {
    assert Thread.holdsLock (this);

    int len = buf.length;
    int offset = 0;
    read_data (buf, offset, len);
  }

  private void handle_exception (Throwable ex) {
    ex.printStackTrace();
  }

  public Event read_event () {

    //System.err.println("read_event");
    //System.err.println("read event from stream");

    // Otherwise we read and return the first event from the stream.
    Event ev = null;
    do {
      // If there are any events already queued up, then return the first
      // event in the queue.
      if (events.size () > 0) {
        ev = (Event) events.removeFirst ();
      } else {
        ev = read_event_from_stream ();
      }

      // If this returned null, there's a reply in the response stream and
      // some other thread is waiting for it, or there is no event and we keep
      // waiting for one...
      if (ev == null) {
        try {Thread.sleep (40); } catch (Exception ex) {}
        //Thread.yield ();
      }

    } while (ev == null);
//    System.err.println("event: " + ev);
    return ev;
  }

  /**
   * Pulls all pending events out of the queue.
   *
   * @return all pending events
   */
  public List pull_all_events () {
    LinkedList l = new LinkedList(events);
    Event e = read_event_from_stream ();
    while (e != null) {
      l.add (e);
    }
    return l;
  }

  /**
   * Reads an event from the input stream of the connection. If there is
   * a reply waiting to be fetched, this returns <code>null</code>.
   *
   * @return the next event from the stream
   */
  private synchronized Event read_event_from_stream () {

    int available = 0;
    try {
      available = in.available ();
      //System.err.println("available: " + available);
    } catch (IOException ex) {
      handle_exception (ex);
    }

    if (available == 0)
      return null;

    // We want to look-ahead the first byte to determine the type of the
    // response.
    int code = -1;
    try {
      in.mark (1);
      code = read_int8 ();
      in.reset ();
    } catch (IOException ex) {
      ex.printStackTrace();
    }
    //System.err.println("reading code: " + code + " masked: " + (code & 0x7f));
    code = code & 0x7f; // Remove synthetic mask.
    Event ev = null;
    if (code >= 64 && code <= 127)
      ev = read_extension_event (code);
    else
      ev = read_core_event (code);
    return ev;
  }

  private Event read_core_event (int code) {
    Event ev = null;
    switch (code) {
    case 0:
      read_error ();
      break;
    case 1:
      ev = null;
      break;
    case 2:
      ev = new KeyPress (display, this);
      break;
    case 3:
      ev = new KeyRelease (display, this);
      break;
    case 4:
      ev = new ButtonPress (display, this);
      break;
    case 5:
      ev = new ButtonRelease (display, this);
      break;
    case 6:
      ev = new MotionNotify (display, this);
      break;
    case 7:
      ev = new EnterNotify (display, this);
      break;
    case 8:
      ev = new LeaveNotify (display, this);
      break;
    case 9:
      ev = new FocusIn (display, this);
      break;
    case 10:
      ev = new FocusOut (display, this);
      break;
    case 11:
      ev = new KeymapNotify (display, this);
      break;
    case 12:
      ev = new Expose (display, this);
      break;
    case 13:
      ev = new GraphicsExpose (display, this);
      break;
    case 14:
      ev = new NoExposure (display, this);
      break;
    case 15:
      ev = new VisibilityNotify (display, this);
      break;
    case 16:
      ev = new CreateNotify (display, this);
      break;
    case 17:
      ev = new DestroyNotify (display, this);
      break;
    case 18:
      ev = new UnmapNotify (display, this);
      break;
    case 19:
      ev = new MapNotify (display, this);
      break;
    case 20:
      ev = new MapRequest (display, this);
      break;
    case 21:
      ev = new ReparentNotify (display, this);
      break;
    case 22:
      ev = new ConfigureNotify (display, this);
      break;
    case 23: 
      ev = new ConfigureRequest (display, this);
      break;
    case 24:
      ev = new GravityNotify (display, this);
      break;
    case 25:
      ev = new ResizeRequest (display, this);
      break;
    case 26:
      ev = new CirculateNotify (display, this);
      break;
    case 27:
      ev = new CirculateRequest (display, this);
      break;
    case 28:
      ev = new PropertyNotify (display, this);
      break;
    case 29:
      ev = new SelectionClear (display, this);
      break;
    case 30:
      ev = new SelectionRequest (display, this);
      break;
    case 31:
      ev = new SelectionNotify (display, this);
      break;
    case 32:
      ev = new ColormapNotify (display, this);
      break;
    case 33:
      ev = new ClientMessage (display, this);
      break;
    case 34:
      ev = new MappingNotify (display, this);
      break;
    default:
      throw new java.lang.Error ("Unsupported core event code: " + code);
    }
    return ev;
  }

  private Event read_extension_event (int code) {
    EventFactory fac = display.extension_event_factories [code - 64];
    if (fac == null)
      throw new java.lang.Error("Unsuppored extension event: " + code);
    return fac.build (display, this, code);
  }

  /**
   * Flushes the currently pending request and starts reading the reply. The specified sequence
   * number is used to check the reply sequence number.
   *
   * @param seq_no the sequence number of the request
   *
   * @return the input stream for reading the reply
   */
  public void read_reply (RequestOutputStream out) {

    // When reading a reply, the calling thread must hold a lock on both
    // the input and the output stream, otherwise we might end up doing
    // nasty stuff.

    assert Thread.holdsLock (this);
    assert Thread.holdsLock (out);


    // Flush the current request.
    // DON'T use plain send() because this could trigger a round-trip check
    // which would mess up with the reply.
    out.send_impl();
    out.flush();

    int exp_seq_no = out.seq_number;

    // Fetch all events and errors that may come before the reply.
    int code = -1;
    do {
      try {
        mark (1);
        code = read_int8 ();
        reset ();
      } catch (IOException ex) {
        handle_exception (ex);
      }
      if (code == 0) // Error.
        read_error ();
      else if (code > 1) { // Event.
        Event ev = read_event_from_stream ();
        if (ev != null)
          events.addLast (ev);
      }// else // Reply or Exception.
    } while (code != 1);
    // Check reply header, especially make sure that the sequence codes match.
    try {
      mark (4);
      int reply = read_int8 ();
      assert reply == 1 : "Reply code must be 1 but is: " + reply;
      skip (1);
      int seq_no = read_int16 ();
      assert (exp_seq_no == seq_no) : "expected sequence number: " + exp_seq_no
                                      + " got sequence number: " + seq_no;
      reset ();
    } catch (IOException ex) {
      handle_exception (ex);
    }

    // Now the calling thread can safely read the reply.
  }


  /**
   * Reads an X error from the stream.
   */
  private void read_error () {
    int reply = read_int8 ();
    assert reply == 0;
    int code = read_int8 ();
    int seq_no = read_int16 ();
    int bad_value = read_int32 ();
    int minor_opcode = read_int16 ();
    int major_opcode = read_int8 ();
    skip (21);
    if (code >= 128 && code <= 255)
      throw build_extension_error (display, code, seq_no, bad_value,
                                   minor_opcode, major_opcode);

    gnu.x11.Error err = new gnu.x11.Error (display,
                                           gnu.x11.Error.ERROR_STRINGS [code],
                                           code, seq_no, bad_value,
                                           minor_opcode, major_opcode);
    throw err;
  }

  private Error build_extension_error (Display display, int code, int seq_no,
                                       int bad, int minor_opcode,
                                       int major_opcode) {

    gnu.x11.extension.ErrorFactory factory = display
    .extension_error_factories [code-128];
                                             
    if (factory == null)
      throw new java.lang.Error ("Unsupported extension error: " + code);

    return factory.build (display, code, seq_no, bad,
                          minor_opcode, major_opcode);
  }
}
