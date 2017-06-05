package gnu.x11.extension;

import gnu.x11.ResponseInputStream;


public interface EventFactory {
  gnu.x11.event.Event build (gnu.x11.Display display, ResponseInputStream i,
                             int code); 
}
