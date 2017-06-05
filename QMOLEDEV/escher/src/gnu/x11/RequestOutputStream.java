package gnu.x11;

import gnu.x11.extension.glx.GLXCommand;

import java.io.FilterOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.Timer;
import java.util.TimerTask;

/**
 * Used to create and manage requests to the X server.
 *
 * @author Roman Kennke (roman@kennke.org)
 */
public class RequestOutputStream extends FilterOutputStream {

  public enum SendMode {
    ASYNCHRONOUS, SYNCHRONOUS, ROUND_TRIP
  }

  protected static final String FLUSH_THREAD_NAME = "FLUSH REQUEST THREAD";
  
  /** 
   * TimerTask for forcing flush of this RequestOutputStream
   * after at most FLUSH_TIMER_DELAY milliseconds.
   */
  private class RequestTimerTask extends TimerTask {

      @Override
      public void run() {

          synchronized (RequestOutputStream.this) {
              flushPending();
          }
      }
      
  };
  
  /**
   * The buffers size.
   */
  public enum Buffer {
      DEFAULT_BUFFER_SIZE(512),
      MAX_BUFFER_SIZE(8192),
      FLUSH_THRESHOLD(64);
      
      private int code;
      
      Buffer(int cd) {
          this.code =cd;
      }
      
      public int getSize() {
          return code;
      }
  }
  
  /**
   * Maximum delay time between two flush request.
   */
  private static final int FLUSH_TIMER_DELAY = 50;

  /**
   * Defines the GLX opcode as returned by the server, in case
   * we are using the GLX extension.
   */
  private int glxMajorOpcode = -1;
  
  /**
   * Flush Timer.
   */
  private Timer flushTimer = null;
    
  /**
   * TimerTask used by the flushTimer Timer.
   */
  private TimerTask timerTask = null;
  
  /**
   * The request buffer. It always holds the current request. This can be
   * accessed directly for modifications, like when the current request
   * can be aggregated.
   */
  private byte [] buffer;

  /**
   * The current write index in the buffer. This always points to the next
   * free location inside the buffer.
   */
  public int index;

  /**
   * The index into the current request.
   */
  private int requestIndex;

  /**
   * The request object. This is written to the stream when flushing.
   */
  // TODO: Restrict visibility.
  RequestObject requestObject;

  /**
   * The sequence number of the current request.
   */
  private int seqNumber;

  /**
   * The send mode for this connection. Either {@link SendMode#ASYNCHRONOUS},
   * {@link SendMode#SYNCHRONOUS} or {@link SendMode#ROUND_TRIP}.
   */
  private SendMode sendMode;

  /**
   * The associated display.
   */
  private Display display;

  /**
   * Creates a new RequestOutputStream with the specified sink and a default
   * buffer size.
   *
   * @param sink the output stream to write to
   */
  RequestOutputStream (OutputStream sink, Display d) {
    this (sink, Buffer.DEFAULT_BUFFER_SIZE.getSize(), d);
  }

  /**
   * Creates a new RequestOutputStream that writes to the specified output
   * stream and has a maximum request buffer size of <code>size</code>.
   *
   * @param sink the output stream to write to
   * @param size the buffer size
   */
  RequestOutputStream (OutputStream sink, int size, Display d) {
    super (sink);
    buffer = new byte [size];
    seqNumber = 0;
    display = d;
    sendMode = getDefaultSendMode ();
    
    this.timerTask = new RequestTimerTask();
    this.flushTimer = new Timer(FLUSH_THREAD_NAME, true);
  }

  /**
   * Determines the default send mode. This can be set by the system property
   * escher.send_mode that can have the values SYNCRONOUS, ASYNCRONOUS
   * and ROUND_TRIP. If no such property is specified, the send mode is set
   * to ASYNCRONOUS, which is the recommended mode for applications. The
   * other two modes are useful for debugging.
   *
   * @return the default send mode
   */
  private SendMode getDefaultSendMode () {
    String prop = System.getProperty ("escher.send_mode", "ASYNCHRONOUS");
    SendMode sm;
    if (prop.equalsIgnoreCase ("SYNCHRONOUS")) {
      sm = SendMode.SYNCHRONOUS;
    } else if (prop.equalsIgnoreCase ("ROUND_TRIP")) {
      sm = SendMode.ROUND_TRIP;
    } else {
      sm = SendMode.ASYNCHRONOUS;
    }
    return sm;
  }

  /**
   * Changes the buffer size.
   *
   * @param size the new buffer size
   *
   * @return the actually used buffer size
   */
  public synchronized int setBufferSize (int size) {
    // First flush all possibly pending request data.
    if (index > 0) {
      System.err.println("WARNING: Unflushed request data.");
      flush ();
    }
    int actual_size = Math.min(size, Buffer.MAX_BUFFER_SIZE.getSize());
    buffer = new byte [actual_size];
    index = 0;
    return actual_size;
  }
 
  /**
   *  Begins a new GLX request. This flushes all pending request data.
   * 
   * @param command
   */
  public void beginGLXRequest(GLXCommand command) {
      
      beginRequest(this.glxMajorOpcode, command.getOpcode(),
                    command.getLength());
  }
  
  /**
   * Begins a new X11 Core request.
   * 
   * @param command
   */
  public void beginX11CoreRequest(X11CoreCommand command, int secondField) {
      
      this.beginRequest(command.getOpcode(), secondField, command.getLength());
  }
  
  /**
   * Begins a new request. This flushes all pending request data.
   *
   * @param opcode the opcode for the request
   * @param secondField the second field for the request
   * @param requestLength the length of the request
   */
  @Deprecated
  public void beginRequest (int opcode, int secondField, int requestLength) {

    assert Thread.holdsLock (this);
 
    // Send pending request.
    sendPendingRequest();

    if (this.timerTask != null) {
        this.timerTask.cancel();
        this.timerTask = null;
    }
    
    this.seqNumber = (this.seqNumber + 1) & 0xffff;
    
    if (buffer.length - index < requestLength * 4) {
      flush();
    }

    writeInt8 (opcode);
    writeInt8 (secondField);
    writeInt16 (requestLength);
  }

  boolean sendPendingRequest() {
      
      if (requestObject != null || index > requestIndex) {
          send ();
          return true;
      }
    
      return false;
  }

  /**
   * Sends the current request to the underlying stream, without necessarily
   * flushing the stream.
   */
  public void send () {

    sendImpl ();
    if (sendMode == SendMode.ROUND_TRIP) {
      doRoundtrip ();
    }
  }

  /**
   * Performs the same operation as {@link #send()} but without possibly
   * doing a round-trip check.
   */
  void sendImpl () {

    assert Thread.holdsLock (this);

    if (requestObject != null) {
      requestObject.write (this);
      requestObject = null;
    }
    if (index > requestIndex) {
      // Possibly pad request.
      int pad = pad (index - requestIndex);
      if (pad != 0)
        skip (pad);
      requestIndex = index;
      if (index > (buffer.length - Buffer.FLUSH_THRESHOLD.getSize())
          || sendMode == SendMode.SYNCHRONOUS
          || sendMode == SendMode.ROUND_TRIP) {
        flush ();
      }

      assert this.timerTask == null;
      this.timerTask = new RequestTimerTask();
      this.flushTimer.schedule(timerTask, FLUSH_TIMER_DELAY);
      
    }
    
  }

  /**
   * This forces a reply from the X server by sending an input focus request.
   * This is used for the {@link SendMode#ROUND_TRIP} mode, which is
   * very useful for debugging because the X errors can be traced to their
   * corresponding calls.
   */
  private void doRoundtrip () {
    display.getInput().inputFocus ();
  }

  /**
   * Returns the opcode of the current request.
   *
   * @return the opcode of the current request
   */
  public int currentOpcode () {
    return index > requestIndex ? buffer [requestIndex] : -1;
  }

  /**
   * Sets the write index to <code>i</code>.
   *
   * @param i the write index to set
   */
  public void setIndex (int i) {
    index = i + requestIndex;
  }

  /**
   * Returns the current index in the current request buffer.
   *
   * @return the current index in the current request buffer
   */
  public int getIndex() {
      return index - requestIndex;
  }

  /**
   * Writes one byte to the stream.
   *
   * @param v
   */
  public void write (int v) throws IOException {
    assert Thread.holdsLock (this);
    buffer [index] = (byte) v;
    index++;
  }

  /**
   * Writes the specified data to the stream.
   *
   * @param b the data to write
   */
  public void write (byte [] b) {
    assert Thread.holdsLock (this);
    System.arraycopy (b, 0, buffer, index, b.length);
    index += b.length;
  }

  /**
   * Writes the specified data to the stream.
   *
   * @param b the data to write
   * @param offs the start offset in the data array
   * @param len the length of the data to write
   */
  public void write (byte [] b, int offs, int len) {
    assert Thread.holdsLock (this);
    System.arraycopy (b, offs, buffer, index, len);
    index += len;
  }

  /**
   * Flushes all the pending request data to the underlying stream.
   */
  public synchronized void flush () {
    try {

      if (requestObject != null) {
        //System.err.println("request object: " + request_object);
        requestObject.write (this);
        requestObject = null;
      }
      if (index > 0) {
        // Possibly pad request.
        int pad = pad (index - requestIndex);
        if (pad != 0)
          skip (pad);
        try {
          out.write (buffer, 0, index);
        } catch (IOException ex) {
          handleException (ex);
        }
        index = 0;
        requestIndex = 0;
      }

      out.flush ();

    } catch (IOException ex) {
      handleException (ex);
    }
  }

  public void flushPending () {
      this.flush();
  }
  
  public void writeBool (boolean b) {
    assert Thread.holdsLock (this);
    int v = (b ? 1 : 0);
    buffer [index] = (byte) (v);
    index++;
  }

  /**
   * Writes an INT8 value to the stream.
   *
   * @param v the value to write
   */
  public void writeInt8 (int v) {
    assert Thread.holdsLock (this);
    buffer [index] = (byte) (v);
    index++;
  }

  /**
   * Writes an INT16 value to the stream.
   *
   * @param v the value to write
   */
  public void writeInt16 (int v) {
    assert Thread.holdsLock (this);
    buffer [index] = (byte) (v >> 8);
    index++;
    buffer [index] = (byte) v;
    index++;
  }

  /**
   * Writes an INT32 value to the stream.
   *
   * @param v the value to write
   */
  public void writeInt32 (int v) {
    assert Thread.holdsLock (this);
    buffer [index] = (byte) (v >> 24);
    index++;
    buffer [index] = (byte) (v >> 16);
    index++;
    buffer [index] = (byte) (v >> 8);
    index++;
    buffer [index] = (byte) v;
    index++;
  }

  /**
   * Returns the INT32 value at the specified index in the buffer inside
   * the current request.
   *
   * @param index the index
   *
   * @return the INT32 value at the specified index
   */
  public int getInt32 (int index) {
    int req_index = index + requestIndex;
    int int32 = (buffer[req_index] << 24) | (buffer[req_index + 1] << 16)
                | (buffer[req_index + 2] << 8) | buffer[req_index + 3];
    return int32;
  }

  public void writeFloat (float f) {
    assert Thread.holdsLock (this);

    int v = Float.floatToIntBits (f);
    
    buffer [index] = (byte) (v >> 24);
    index++;
    buffer [index] = (byte) (v >> 16);
    index++;
    buffer [index] = (byte) (v >> 8);
    index++;
    buffer [index] = (byte) v;
    index++;
  }

  public void writeDouble (double d) {
    assert Thread.holdsLock (this);

    long v = Double.doubleToLongBits (d);
    
    buffer [index] = (byte) (v >> 56);
    index++;
    buffer [index] = (byte) (v >> 48);
    index++;
    buffer [index] = (byte) (v >> 40);
    index++;
    buffer [index] = (byte) (v >> 32);
    index++;
    buffer [index] = (byte) (v >> 24);
    index++;
    buffer [index] = (byte) (v >> 16);
    index++;
    buffer [index] = (byte) (v >> 8);
    index++;
    buffer [index] = (byte) v;
    index++;
  }

  /**
   * Writes a STRING8 value to the stream.
   *
   * @param s the string to write
   */
  public void writeString8 (String s) {
    assert Thread.holdsLock (this);
    write (s.getBytes ());
  }

  /**
   * Writes a STRING16 to the stream.
   *
   * @param s the string to write
   */
  public void writeString16 (String s) {
    assert Thread.holdsLock (this);
    char [] chars = s.toCharArray();
    int len = chars.length;
    for (int i = 0; i < len; i++) {
      writeInt16 (chars [i]);
    }
  }

  public void writeBytes (byte [] b) {
    assert Thread.holdsLock (this);
    write (b);
  }

  public long skip (long n) {
    assert Thread.holdsLock (this);
    index += n;
    return n;
  }

  /**
   * Skips p unused bytes, where p is pad(n). pad(n) is the number of
   * bytes that are needed to fill a block multiple of 4.
   *
   * @param n the number to be padded
   */
  public void writePad (int n) {
    assert Thread.holdsLock (this);
    skip (pad (n));
  }

  /**
   * Returns the number of bytes that are needed to pad <code>n</code> bytes
   * to fill a multiple of four.
   *
   * @param n the number of bytes the pad
   *
   * @return the number of pad bytes needed
   */
  public static int pad (int n) {
    int pad = n % 4;
    if (pad > 0)
      pad = 4 - pad;
    return pad;
  }

  /**
   * Returns the opcode of the current request or -1 if there is no request
   * pending.
   *
   * @return the opcode of the current request or -1 if there is no request
   *         pending
   */
  public int opcode () {
    return index > 0 ? buffer [0] : -1;
  }

  /**
   * Determines if the buffer has room for the specified number of bytes.
   *
   * @param num_bytes the number of bytes
   *
   * @return <code>true</code> if the buffer has space for the specified number
   *         of bytes, <code>false</code> otherwise
   */
  public boolean fits (int num_bytes) {
    return index + num_bytes <= buffer.length;
  }

  /**
   * Updates the length field of the request to reflect the current length.
   */
  public void updateLength () {
    int len = (index + 3) / 4;
    buffer [2] = (byte) (len >> 8);
    buffer [3] = (byte) (len);
  }

  /**
   * Handles exceptions that my occur during IO operations.
   *
   * @param ex the exception to handle
   */
  private void handleException (Throwable ex) {
    ex.printStackTrace();
  }

  public void increaseLength (int i) {
    int l = (((int) (buffer[requestIndex + 2] & 0xff)) << 8)
            + (buffer[requestIndex + 3] & 0xff);
    l += i;
    buffer [requestIndex + 2] = (byte) (l >> 8);
    buffer [requestIndex + 3] = (byte) l;

  }

  public int getSequenceNumber () {
    return this.seqNumber;
  }

  /**
   * Returns the current request object, if there is any, otherwise
   * <code>null</code>. Note that most requests are not implemented using
   * request objects.
   *
   * @return the current request object, if there is any, otherwise
   *         <code>null</code>
   */
  public RequestObject getRequestObject() {
    return requestObject;
  }

  public void setRequestObject(RequestObject ro) {
    requestObject = ro;
  }

  public int getBufferLength() {
    return this.buffer.length;
  }
  
  /**
   * Set the opcode returned by the server for the GLX extension. 
   * @param majorOpcode
   */
  public void setGLXMajorOpcode(int majorOpcode) {
          
      this.glxMajorOpcode = majorOpcode;
  }
}
