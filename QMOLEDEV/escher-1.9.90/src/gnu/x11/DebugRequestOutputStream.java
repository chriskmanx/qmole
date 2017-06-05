package gnu.x11;

import java.io.OutputStream;

import java.util.logging.ConsoleHandler;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Wrapper class for debugging {@link RequestOutputStream}. 
 * 
 * @author Mario Torre <neugens@aicas.com>
 */
class DebugRequestOutputStream extends RequestOutputStream {

    private static final String CLASS_NAME =
        DebugRequestOutputStream.class.getName();

    private static final Logger logger;
    static {
        logger = java.util.logging.Logger.getLogger("gnu.x11.DebugRequestOutputStream");

        logger.setLevel(Level.ALL);
        Handler h = new ConsoleHandler();
        h.setLevel(Level.FINEST);
        logger.addHandler(h);

        try {
            // unless explicitly asked to do otherwise, we set the send_mode to
            // SYNCHRONOUS for debugging.
            String sendMode = System.getProperty("escher.send_mode", "ROUND_TRIP");
            System.setProperty("escher.send_mode", sendMode);

        } catch (SecurityException e) {
            // ok, not allowed to get/set sendMode...
        }
    }

    @Override
    public void beginRequest(int opcode, int second_field, int request_length) {

        // begin_request will increment the sequence number, but what we get
        // here
        // is still the not yet update sequence number.
        int sequenceNumber = getSequenceNumber() + 1;
        String message = "-----> BEGIN NEW REQUEST [opcode: " + opcode
                + " | seq_number: " + sequenceNumber + " | second field: "
                + second_field + " | request length: " + request_length
                + " ] <-----";

        logger.logp(Level.FINEST, CLASS_NAME, "begin_request", message);

        super.beginRequest(opcode, second_field, request_length);
    }

    @Override
    public synchronized void flush() {

        int sequenceNumber = getSequenceNumber();
        String message = "[opcode: " + super.opcode() + " | seq_number: "
                + sequenceNumber + "]";

        logger.logp(Level.FINEST, CLASS_NAME, "flush", message);

        super.flush();
    }

    @Override
    public int getInt32(int index) {

        int sequenceNumber = getSequenceNumber();
        String message = "[get_int32 - opcode: " + super.opcode()
                + " | seq_number: " + sequenceNumber + " | index: " + index
                + " ]";

        logger.logp(Level.FINEST, CLASS_NAME, "get_int32", message);

        return super.getInt32(index);
    }

    @Override
    public void increaseLength(int i) {

        int sequenceNumber = getSequenceNumber();
        String message = "[opcode: " + super.opcode() + " | seq_number: "
                + sequenceNumber + " | i: " + i + " ]";

        logger.logp(Level.FINEST, CLASS_NAME, "increase_length", message);

        super.increaseLength(i);
    }

    @Override
    void sendImpl() {

        int sequenceNumber = getSequenceNumber();
        String message = "[opcode: " + super.opcode() + " | seq_number: "
                + sequenceNumber + " | index: " + getIndex() + " ]";

        logger.logp(Level.FINEST, CLASS_NAME, "send_impl", message);

        super.sendImpl();
    }

    /*
     * (non-Javadoc)
     * 
     * @see gnu.x11.RequestOutputStream#set_buffer_size(int)
     */
    @Override
    public synchronized int setBufferSize(int size) {

        int sequenceNumber = getSequenceNumber();
        String message = "[opcode: " + super.opcode() + " | seq_number: "
                + sequenceNumber + " | size: " + size + " ]";

        logger.logp(Level.FINEST, CLASS_NAME, "set_buffer_size", message);

        return super.setBufferSize(size);
    }

    @Override
    public void setIndex(int i) {

        int sequenceNumber = getSequenceNumber();
        String message = "[opcode: " + super.opcode() + " | seq_number: "
                + sequenceNumber + " | i: " + i + " ]";

        logger.logp(Level.FINEST, CLASS_NAME, "set_index", message);

        super.setIndex(i);
    }

    @Override
    public long skip(long n) {

        int sequenceNumber = getSequenceNumber();
        String message = "[opcode: " + super.opcode() + " | seq_number: "
                + sequenceNumber + " | n: " + n + " ]";

        logger.logp(Level.FINEST, CLASS_NAME, "skip", message);

        return super.skip(n);
    }

    @Override
    public void updateLength() {

        int sequenceNumber = getSequenceNumber();
        String message = "[opcode: " + super.opcode() + " | seq_number: "
                + sequenceNumber + " ]";

        logger.logp(Level.FINEST, CLASS_NAME, "update_length", message);

        super.updateLength();
    }

    @Override
    public void writeBool(boolean b) {

        int sequenceNumber = getSequenceNumber();
        String message = "[opcode: " + super.opcode() + " | seq_number: "
                + sequenceNumber + " ]";

        logger.logp(Level.FINEST, CLASS_NAME, "write_bool", message);

        super.writeBool(b);
    }

    @Override
    public void writeBytes(byte[] b) {

        int sequenceNumber = getSequenceNumber();
        String message = "[opcode: " + super.opcode() + " | seq_number: "
                + sequenceNumber + " | byte: " + b + " ]";

        logger.logp(Level.FINEST, CLASS_NAME, "write_bytes", message);

        super.writeBytes(b);

    }

    @Override
    public void writeDouble(double d) {

        int sequenceNumber = getSequenceNumber();
        String message = "[opcode: " + super.opcode() + " | seq_number: "
                + sequenceNumber + " | double: " + d + " ]";

        logger.logp(Level.FINEST, CLASS_NAME, "write_double", message);

        super.writeDouble(d);
    }

    @Override
    public void writeFloat(float f) {

        int sequenceNumber = getSequenceNumber();
        String message = "[opcode: " + super.opcode() + " | seq_number: "
                + sequenceNumber + " | float: " + f + " ]";

        logger.logp(Level.FINEST, CLASS_NAME, "write_float", message);

        super.writeFloat(f);

    }

    @Override
    public void writeInt16(int v) {

        int sequenceNumber = getSequenceNumber();
        String message = "[opcode: " + super.opcode() + " | seq_number: "
                + sequenceNumber + " | int16: " + v + " ]";

        logger.logp(Level.FINEST, CLASS_NAME, "write_int16", message);

        super.writeInt16(v);
    }

    @Override
    public void writeInt32(int v) {

        int sequenceNumber = getSequenceNumber();
        String message = "[opcode: " + super.opcode() + " | seq_number: "
                + sequenceNumber + " | int32: " + v + " ]";

        logger.logp(Level.FINEST, CLASS_NAME, "write_int32", message);

        super.writeInt32(v);
    }

    @Override
    public void writeInt8(int v) {

        int sequenceNumber = getSequenceNumber();
        String message = "[opcode: " + super.opcode() + " | seq_number: "
                + sequenceNumber + " | int8: " + v + " ]";

        logger.logp(Level.FINEST, CLASS_NAME, "write_int8", message);

        super.writeInt8(v);
    }

    @Override
    public void writePad(int n) {

        int sequenceNumber = getSequenceNumber();
        String message = "[opcode: " + super.opcode() + " | seq_number: "
                + sequenceNumber + " | n: " + n + " ]";

        logger.logp(Level.FINEST, CLASS_NAME, "write_pad", message);

        super.writePad(n);
    }

    @Override
    public void writeString16(String s) {

        int sequenceNumber = getSequenceNumber();
        String message = "[opcode: " + super.opcode() + " | seq_number: "
                + sequenceNumber + " | String16: " + s + " ]";

        logger.logp(Level.FINEST, CLASS_NAME, "write_string16", message);

        super.writeString16(s);
    }

    @Override
    public void writeString8(String s) {

        int sequenceNumber = getSequenceNumber();
        String message = "[opcode: " + super.opcode() + " | seq_number: "
                + sequenceNumber + " | String8: " + s + " ]";

        logger.logp(Level.FINEST, CLASS_NAME, "write_string8", message);

        super.writeString8(s);
    }

    public DebugRequestOutputStream(OutputStream sink, Display d) {

        super(sink, d);
    }

    public DebugRequestOutputStream(OutputStream sink, int size, Display d) {

        super(sink, size, d);
    }

    @Override
    public void flushPending() {

        int sequenceNumber = getSequenceNumber();
        String message = "!!!!! " + FLUSH_THREAD_NAME +
                         " [ seq_number: " + sequenceNumber + " ] !!!!!";

        logger.logp(Level.FINEST, CLASS_NAME, "flushPending", message);

    }
    
    @Override
    boolean sendPendingRequest() {

        int sequenceNumber = getSequenceNumber();
        
        String message = "-----> SEARCH_PENDING_REQUEST [ " + " seq_number: "
                         + sequenceNumber + " ] <-----";
        
        logger.logp(Level.FINEST, CLASS_NAME, "sendPendingRequest", message);
        
        boolean sent = super.sendPendingRequest();
        
        if (sent) {
            message = "-----> PENDING_REQUEST_SENT [ " + " seq_number: "
                      + sequenceNumber + " ] <-----";
            logger.logp(Level.FINEST, CLASS_NAME, "sendPendingRequest",
                        message);
        } else {
            message = "-----> NO_PENDING_REQUEST [ " + " seq_number: "
                      + sequenceNumber + " ] <-----";
            logger.logp(Level.FINEST, CLASS_NAME, "sendPendingRequest",
                        message);
        }
        
        return sent;
    }
}
