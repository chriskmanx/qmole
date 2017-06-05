package gnu.x11.event;

import java.util.HashMap;
import java.util.Map;

/** Event Code List, got it from XMonad */
public enum EventCode {
    KEY_PRESS(2),
    KEY_RELEASE(3),
    BUTTON_PRESS(4),
    BUTTON_RELEASE(5),
    MOTION_NOTIFY(6),
    ENTER_NOTIFY(7),
    LEAVE_NOTIFY(8),
    FOCUS_IN(9),
    FOCUS_OUT(10),
    KEYMAP_NOTIFY(11),
    EXPOSE(12),
    GRAPHICS_EXPOSE(13),
    NO_EXPOSE(14),
    VISIBILITY_NOTIFY(15),
    CREATE_NOTIFY(16),
    DESTROY_NOTIFY(17),
    UNMAP_NOTIFY(18),
    MAP_NOTIFY(19),
    MAP_REQUEST(20),
    REPARENT_NOTIFY(21),
    CONFIGURE_NOTIFY(22),
    CONFIGURE_REQUEST(23),
    GRAVITY_NOTIFY(24),
    RESIZE_REQUEST(25),
    CIRCULATE_NOTIFY(26),
    CIRCULATE_REQUEST(27),
    PROPERTY_NOTIFY(28),
    SELECTION_CLEAR(29),
    SELECTION_REQUEST(30),
    SELECTION_NOTIFY(31),
    COLORMAP_NOTIFY(32),
    CLIENT_MESSAGE(33),
    MAPPING_NOTIFY(34),
    LAST_EVENT(36);
    
    private int code;
    
    EventCode(int code) {
        this.code = code;
    }
    
    public int getCode() {
        return code;
    }

    private static Map<Integer, EventCode> eventCodes;
    
    private static Map<Integer, EventCode> getEventCodes() {
        if (eventCodes == null) {
            eventCodes = new HashMap<Integer, EventCode>();
            for (EventCode code : EventCode.values())
                eventCodes.put(code.getCode(), code);
        }
        
        return eventCodes;
    }
    
    public static EventCode getEventByID(int id) {
        EventCode event = getEventCodes().get(id);
        return (event == null) ? event : LAST_EVENT;
      }
    
    public EventCode and(int mask) {
        int andMask = this.getCode() & mask;
        return getEventCodes().get(andMask);
    }
}
