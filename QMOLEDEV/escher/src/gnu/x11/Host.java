package gnu.x11;


public class Host {
    
    private InternetFamily family;
    private byte[] address;
    
    public enum InternetFamily {
        INTERNET(0),
        DECNET(1),
        CHAOS(2);
        
        private int code;
        
        InternetFamily(int cd) {
            this.code =cd;
        }
        
        public int getCode() {
            return code;
        }
        
        public static InternetFamily getFamily(int code) {
            switch (code)
            {
                case 0: return InternetFamily.INTERNET;
                case 1: return InternetFamily.DECNET;
                case 2: return InternetFamily.CHAOS;
                
                default: return InternetFamily.INTERNET;
            }
        }
    }
    
    public enum ChangeOperation {
        INSERT(0),
        DELETE(1);
        
        private int code;
        
        ChangeOperation(int cd) {
            this.code =cd;
        }
        
        public int getCode() {
            return code;
        }
        
        public static ChangeOperation getOperation(int code) {
            return code == 0 ? ChangeOperation.INSERT : ChangeOperation.DELETE;
        }
    }
    
    public enum ForceScreenSaver {
        ACTIVATE(0),
        RESET(1);
        
        private int code;
        
        ForceScreenSaver(int cd) {
            this.code =cd;
        }
        
        public int getCode() {
            return code;
        }
        
        public static ForceScreenSaver getOperation(int code) {
            return code == 0 ? ForceScreenSaver.ACTIVATE : ForceScreenSaver.RESET;
        }
    }

    public enum AccessControl {
        ENABLE(0),
        DISABLED(1);
        
        private int code;
        
        AccessControl(int cd) {
            this.code =cd;
        }
        
        public int getCode() {
            return code;
        }
        
        public static AccessControl getControl(boolean code) {
            return code ? AccessControl.ENABLE : AccessControl.DISABLED;
        }
    }
    
    public enum Shape {
        DESTROY(0),
        RETAIN_PERMANENT(1),
        RETAIN_TEMPORARY(2);
        
        private int code;
        
        Shape(int cd) {
            this.code =cd;
        }
        
        public int getCode() {
            return code;
        }
        
        public static Shape getFamily(int code) {
            switch (code)
            {
                case 0: return Shape.DESTROY;
                case 1: return Shape.RETAIN_PERMANENT;
                case 2: return Shape.RETAIN_TEMPORARY;
                default: return Shape.DESTROY;
            }
        }
    }
    
    /**
     * Reads one Host instance from a ResponseInputStream.
     * 
     * @param in
     *            the input stream to read from
     */
    Host(ResponseInputStream in) {
        family = InternetFamily.getFamily(in.readInt8());
        in.skip(1);
        int addLen = in.readInt16();
        address = new byte[addLen];
        in.readData(address);
        in.pad(addLen);
    }
}
