package gnu.x11.event;

import gnu.x11.Display;
import gnu.x11.RequestOutputStream;
import gnu.x11.ResponseInputStream;
import gnu.x11.Window;

/**
 * X input-related event.
 */
public abstract class Input extends Event {

    private int time;

    private int rootWindowID;

    private int eventWindowID;

    private int childWindowID;

    private int rootX;

    private int rootY;

    private int eventX;

    private int eventY;

    private int state;

    private boolean sameScreen;

    /**
     * Reads the event from the input stream.
     */
    public Input(Display display, ResponseInputStream in) {

        super(display, in);
        
        this.time = in.readInt32();
        this.rootWindowID = in.readInt32();
        this.eventWindowID = in.readInt32();
        this.childWindowID = in.readInt32();
        this.rootX = in.readInt16();
        this.rootY = in.readInt16();
        this.eventX = in.readInt16();
        this.eventY = in.readInt16();
        this.state = in.readInt16();
        this.sameScreen = in.readBool();
        
        in.skip(1); // Unused.
    }

    public Input(Display display, EventCode code) {

        super(display, code);
    }

    public int detail() {

        return detail;
    }

    /**
     * @param time the time to set
     */
    public void setTime(int time) {

        this.time = time;
    }

    /**
     * @return the time
     */
    public int getTime() {

        return time;
    }
    
    /**
     * @param root_window_id the root_window_id to set
     */
    public void setRootWindowID(int root_window_id) {

        this.rootWindowID = root_window_id;
    }

    /**
     * @return the root_window_id
     */
    public int getRootWindowID() {

        return rootWindowID;
    }

    /**
     * @param event_window_id the event_window_id to set
     */
    public void setEventWindowID(int event_window_id) {

        this.eventWindowID = event_window_id;
    }

    /**
     * @return the event_window_id
     */
    public int getEventWindowID() {

        return eventWindowID;
    }

    /**
     * @param same_screen the same_screen to set
     */
    public void setSameScreen(boolean same_screen) {

        this.sameScreen = same_screen;
    }

    /**
     * @return the same_screen
     */
    public boolean isSameScreen() {

        return this.sameScreen();
    }

    /**
     * @deprecated Use {@link #getRootID()} instead
     */
    public int root_id() {

        return getRootWindowID();
    }

    public int getRootID() {

        return root_id();
    }

    /**
     * @deprecated Use {@link #getChildID()} instead
     */
    public int childID() {
        
        return childWindowID;
    }

    public int getChildID() {

        return childID();
    }

    /**
     * @deprecated Use {@link #getRootX()} instead
     */
    public int rootX() {
        
        return rootX;
    }

    public int getRootX() {

        return rootX();
    }

    /**
     * @deprecated Use {@link #getRootY()} instead
     */
    public int rootY() {
        
        return rootY;
    }

    public int getRootY() {

        return rootY();
    }

    /**
     * @deprecated Use {@link #getEventX()} instead
     */
    public int eventX() {
        
        return eventX;
    }

    public int getEventX() {

        return eventX();
    }

    /**
     * @deprecated Use {@link #getEventY()} instead
     */
    public int eventY() {
        
        return eventY;
    }

    public int getEventY() {

        return eventY();
    }

    /**
     * @deprecated Use {@link #getState()} instead
     */
    public int state() {
        
        return state;
    }

    public int getState() {

        return state();
    }

    /**
     * @deprecated use {@link #isSameScreen()} instead
     */
    public boolean sameScreen() {

        return this.sameScreen;
    }

    /**
     * @deprecated Use {@link #getRoot()} instead
     */
    public Window root() {
        return (Window) Window.intern(display, getRootWindowID());
    }

    public Window getRoot() {

        return root();
    }

    /**
     * @deprecated Use {@link #getChild()} instead
     */
    public Window child() {
        
        return (Window) Window.intern(display, childWindowID);
    }

    public Window getChild() {

        return child();
    }

    /**
     * @deprecated Use {@link #setWindow(Window)} instead
     */
    public void set_window(Window w) {

        setEventWindowID(w.getID());
    }

    public void setWindow(Window w) {

        set_window(w);
    }

    /**
     * @deprecated Use {@link #setDetail(int)} instead
     */
    public void set_detail(int d) {
        
        detail = d;
    }

    public void setDetail(int d) {

        set_detail(d);
    }

    /**
     * @deprecated Use {@link #setState(int)} instead
     */
    public void set_state(int s) {
        
        state = s;
    }

    public void setState(int s) {

        set_state(s);
    }

    public void write(RequestOutputStream o) {

        super.write(o);
        o.writeInt32(time);
        o.writeInt32(rootWindowID);
        o.writeInt32(eventWindowID);
        o.writeInt32(childWindowID);
        o.writeInt16(rootX);
        o.writeInt16(rootY);
        o.writeInt16(eventX);
        o.writeInt16(eventY);
        o.writeInt16(state);
        o.writeBool(sameScreen);
        o.skip(1); // Unused.

    }
}
