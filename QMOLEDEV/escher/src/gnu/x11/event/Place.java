package gnu.x11.event;


public enum Place {
    TOP(0),
    BOTTOM(1);
    
    private int code;
    
    Place(int code) {
        this.code = code;
    }
    
    
    public int getCode() {

        return code;
    }
    
    public static Place getByCode(int code) {
        return code == 0 ? TOP : BOTTOM;
    }
}
