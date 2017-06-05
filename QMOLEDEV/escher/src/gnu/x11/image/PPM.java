
package gnu.x11.image;

import gnu.x11.Colormap;
import gnu.x11.Display;
import gnu.x11.EscherUnsupportedScreenBitDepthException;
import gnu.x11.VisualInfo;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class PPM extends ZPixmap { // TODO

    public PPM(Display display, java.io.InputStream in, VisualInfo xVisual)
            throws IOException, EscherUnsupportedScreenBitDepthException {

        // WARNING: very naive parsing

        super(display, xVisual);

        BufferedReader bin = new BufferedReader(new InputStreamReader(in));

        // format

        String format = bin.readLine();
        if (!(format.equals("P6")))
            throw new Error("Unsupported format: " + format);

        // dimension

        String dimension = bin.readLine();
        int index = dimension.indexOf(' ');
        try {
            width = Integer.parseInt(dimension.substring(0, index));
            height = Integer.parseInt(dimension.substring(index + 1, dimension
                    .length()));

        } catch (Exception e) {
            throw new Error("Invalid dimension: " + dimension);
        }

        String color_count = bin.readLine();

        // fill up data

        init();
        Colormap cmap = display.getDefaultColormap();

        for (int y = 0; y < height; y++)
            for (int x = 0; x < width; x++) {
                int r = bin.read();
                int g = bin.read();
                int b = bin.read();

                // FIXME cache and index color
                putPixel(x, y, cmap.allocColor8(r, g, b));
            }
    }
}
