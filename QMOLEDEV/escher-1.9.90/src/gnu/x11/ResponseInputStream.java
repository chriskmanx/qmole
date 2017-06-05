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
    private LinkedList events = new LinkedList();

    /**
     * Creates a new ResponseInputStream.
     * 
     * @param source
     *                the stream to read from
     */
    ResponseInputStream(InputStream source, Display d) {

        super(source);
        this.display = d;
    }

    private Error buildExtensionError(Display display, int code, int seqNo,
            int bad, int minorOpcode, int majorOpcode) {

        gnu.x11.extension.ErrorFactory factory = display.extensionErrorFactories[code - 128];

        if (factory == null) {
            throw new java.lang.Error("Unsupported extension error: " + code);
        }

        return factory.build(display, code, seqNo, bad, minorOpcode,
                majorOpcode);
    }

    private void handleException(Throwable ex) {

        ex.printStackTrace();
    }

    public void pad(int n) {

        assert Thread.holdsLock(this);

        int pad = n % 4;
        if (pad > 0) {
            pad = 4 - pad;
        }
        this.skip(pad);
    }

    /**
     * Pulls all pending events out of the queue.
     * 
     * @return all pending events
     */
    public List pullAllEvents() {

        LinkedList l = new LinkedList(this.events);
        Event e = this.readEventFromStream();
        while (e != null) {
            l.add(e);
        }
        return l;
    }

    public boolean readBool() {

        assert Thread.holdsLock(this);

        boolean v = false;
        try {
            v = this.read() != 0;
        } catch (IOException ex) {
            this.handleException(ex);
        }
        return v;
    }

    /**
     * Reads an (unsigned) byte value from the underlying stream.
     * 
     * @return the byte value
     */
    public int readByte() {

        assert Thread.holdsLock(this);

        int v = -1;
        try {
            v = this.read();
        } catch (IOException ex) {
            this.handleException(ex);
        }
        return v & 0xff;
    }

    private Event readCoreEvent(int code) {

        Event ev = null;
        switch (code) {
        case 0:
            this.readError();
            break;
        case 1:
            ev = null;
            break;
        case 2:
            ev = new KeyPress(this.display, this);
            break;
        case 3:
            ev = new KeyRelease(this.display, this);
            break;
        case 4:
            ev = new ButtonPress(this.display, this);
            break;
        case 5:
            ev = new ButtonRelease(this.display, this);
            break;
        case 6:
            ev = new MotionNotify(this.display, this);
            break;
        case 7:
            ev = new EnterNotify(this.display, this);
            break;
        case 8:
            ev = new LeaveNotify(this.display, this);
            break;
        case 9:
            ev = new FocusIn(this.display, this);
            break;
        case 10:
            ev = new FocusOut(this.display, this);
            break;
        case 11:
            ev = new KeymapNotify(this.display, this);
            break;
        case 12:
            ev = new Expose(this.display, this);
            break;
        case 13:
            ev = new GraphicsExpose(this.display, this);
            break;
        case 14:
            ev = new NoExposure(this.display, this);
            break;
        case 15:
            ev = new VisibilityNotify(this.display, this);
            break;
        case 16:
            ev = new CreateNotify(this.display, this);
            break;
        case 17:
            ev = new DestroyNotify(this.display, this);
            break;
        case 18:
            ev = new UnmapNotify(this.display, this);
            break;
        case 19:
            ev = new MapNotify(this.display, this);
            break;
        case 20:
            ev = new MapRequest(this.display, this);
            break;
        case 21:
            ev = new ReparentNotify(this.display, this);
            break;
        case 22:
            ev = new ConfigureNotify(this.display, this);
            break;
        case 23:
            ev = new ConfigureRequest(this.display, this);
            break;
        case 24:
            ev = new GravityNotify(this.display, this);
            break;
        case 25:
            ev = new ResizeRequest(this.display, this);
            break;
        case 26:
            ev = new CirculateNotify(this.display, this);
            break;
        case 27:
            ev = new CirculateRequest(this.display, this);
            break;
        case 28:
            ev = new PropertyNotify(this.display, this);
            break;
        case 29:
            ev = new SelectionClear(this.display, this);
            break;
        case 30:
            ev = new SelectionRequest(this.display, this);
            break;
        case 31:
            ev = new SelectionNotify(this.display, this);
            break;
        case 32:
            ev = new ColormapNotify(this.display, this);
            break;
        case 33:
            ev = new ClientMessage(this.display, this);
            break;
        case 34:
            ev = new MappingNotify(this.display, this);
            break;
        default:
            throw new java.lang.Error("Unsupported core event code: " + code);
        }
        return ev;
    }

    public void readData(byte[] buf) {

        assert Thread.holdsLock(this);

        int len = buf.length;
        int offset = 0;
        this.readData(buf, offset, len);
    }

    public void readData(byte[] buf, int offset, int len) {

        assert Thread.holdsLock(this);

        try {
            while (len > 0) {
                int numread = this.in.read(buf, offset, len);
                if (numread < 0) {
                    throw new EOFException();
                }
                len -= numread;
                offset += numread;
            }
        } catch (IOException ex) {
            this.handleException(ex);
        }
    }

    /**
     * Reads an X error from the stream.
     */
    private void readError() {

        int reply = this.readInt8();
        assert reply == 0;
        int code = this.readInt8();
        int seq_no = this.readInt16();
        int bad_value = this.readInt32();
        int minor_opcode = this.readInt16();
        int major_opcode = this.readInt8();
        this.skip(21);
        if (code >= 128 && code <= 255) {
            throw this.buildExtensionError(this.display, code, seq_no, bad_value,
                    minor_opcode, major_opcode);
        }

        gnu.x11.Error.ErrorCode error = gnu.x11.Error.ErrorCode.getError(code);
        gnu.x11.Error err = new gnu.x11.Error(this.display,
                error.getErrorMessage(), error, seq_no, bad_value,
                minor_opcode, major_opcode);
        throw err;
    }

    public Event readEvent() {

        // Otherwise we read and return the first event from the stream.
        Event ev = null;
        do {
            // If there are any events already queued up, then return the first
            // event in the queue.
            if (this.events.size() > 0) {
                ev = (Event) this.events.removeFirst();
            } else {
                ev = this.readEventFromStream();
            }

            // If this returned null, there's a reply in the response stream and
            // some other thread is waiting for it, or there is no event and we
            // keep
            // waiting for one...
            if (ev == null) {
                try {
                    Thread.sleep(40);
                } catch (Exception ex) {
                }
                // Thread.yield ();
            }

        } while (ev == null);
        // System.err.println("event: " + ev);
        return ev;
    }

    /**
     * Reads an event from the input stream of the connection. If there is a
     * reply waiting to be fetched, this returns <code>null</code>.
     * 
     * @return the next event from the stream
     */
    private synchronized Event readEventFromStream() {

        int available = 0;
        try {
            available = this.in.available();
            // System.err.println("available: " + available);
        } catch (IOException ex) {
            this.handleException(ex);
        }

        if (available == 0) {
            return null;
        }

        // We want to look-ahead the first byte to determine the type of the
        // response.
        int code = -1;
        try {
            this.in.mark(1);
            code = this.readInt8();
            this.in.reset();
        } catch (IOException ex) {
            ex.printStackTrace();
        }
        // System.err.println("reading code: " + code + " masked: " + (code &
        // 0x7f));
        code = code & 0x7f; // Remove synthetic mask.
        Event ev = null;
        if (code >= 64 && code <= 127) {
            ev = this.readExtensionEvent(code);
        } else {
            ev = this.readCoreEvent(code);
        }
        return ev;
    }

    private Event readExtensionEvent(int code) {

        EventFactory fac = this.display.extensionEventFactories[code - 64];
        if (fac == null) {
            throw new java.lang.Error("Unsuppored extension event: " + code);
        }
        return fac.build(this.display, this, code);
    }

    public float readFloat32() {

        int bits = this.readInt32();
        float v = Float.intBitsToFloat(bits);
        return v;
    }

    public double readFloat64() {

        long bits = this.readInt64();
        double v = Double.longBitsToDouble(bits);
        return v;
    }

    /**
     * Reads an INT16 value from the stream.
     * 
     * @return the value
     */
    public int readInt16() {

        assert Thread.holdsLock(this);

        int v = -1;
        try {
            v = (this.read() << 8) | this.read();
        } catch (IOException ex) {
            this.handleException(ex);
        }
        return v;
    }

    /**
     * Reads an INT32 value from the stream.
     * 
     * @return the value
     */
    public int readInt32() {

        assert Thread.holdsLock(this);

        int v = -1;
        try {
            v = (this.read() << 24) | (this.read() << 16) | (this.read() << 8) | this.read();
        } catch (IOException ex) {
            this.handleException(ex);
        }
        return v;
    }

    /**
     * Reads an INT32 value from the stream.
     * 
     * @return the value
     */
    public long readInt64() {

        assert Thread.holdsLock(this);

        long v = -1;
        try {
            v = (this.read() << 56) | (this.read() << 48) | (this.read() << 40)
                    | (this.read() << 32) | (this.read() << 24) | (this.read() << 16)
                    | (this.read() << 8) | this.read();
        } catch (IOException ex) {
            this.handleException(ex);
        }
        return v;
    }

    public int readInt8() {

        assert Thread.holdsLock(this);

        int v = -1;
        try {
            v = this.read();
        } catch (IOException ex) {
            this.handleException(ex);
        }
        return v;
    }

    /**
     * Flushes the currently pending request and starts reading the reply. The
     * specified sequence number is used to check the reply sequence number.
     * 
     * @param seq_no
     *                the sequence number of the request
     * 
     * @return the input stream for reading the reply
     */
    public void readReply(RequestOutputStream out) {

        // When reading a reply, the calling thread must hold a lock on both
        // the input and the output stream, otherwise we might end up doing
        // nasty stuff.

        assert Thread.holdsLock(this);
        assert Thread.holdsLock(out);

        // Flush the current request.
        // DON'T use plain send() because this could trigger a round-trip check
        // which would mess up with the reply.
        out.sendImpl();
        out.flush();

        int exp_seq_no = out.getSequenceNumber();

        // Fetch all events and errors that may come before the reply.
        int code = -1;
        do {
            try {
                this.mark(1);
                code = this.readInt8();
                this.reset();
            } catch (IOException ex) {
                this.handleException(ex);
            }
            if (code == 0) {
                this.readError();
            } else if (code > 1) { // Event.
                Event ev = this.readEventFromStream();
                if (ev != null) {
                    this.events.addLast(ev);
                }
            }// else // Reply or Exception.
        } while (code != 1);
        // Check reply header, especially make sure that the sequence codes
        // match.
        try {
            this.mark(4);
            int reply = this.readInt8();
            assert reply == 1 : "Reply code must be 1 but is: " + reply;
            this.skip(1);
            int seq_no = this.readInt16();
            assert (exp_seq_no == seq_no) : "expected sequence number: "
                    + exp_seq_no + " got sequence number: " + seq_no;
            this.reset();
        } catch (IOException ex) {
            this.handleException(ex);
        }

        // Now the calling thread can safely read the reply.
    }

    public String readString8(int len) {

        assert Thread.holdsLock(this);

        byte[] buf = new byte[len];
        this.readData(buf);
        String s = new String(buf);
        return s;
    }

    /**
     * Skips n bytes in the stream.
     * 
     * @param n
     *         the number of bytes to skip
     * 
     * @return the actual number of bytes skipped
     */
    @Override
    public long skip(long n) {

        assert Thread.holdsLock(this);

        long s = 0;
        try {
            while (s < n) {
                s += super.skip(n - s);
            }
        } catch (Exception ex) {
            this.handleException(ex);
        }
        return s;
    }
}
