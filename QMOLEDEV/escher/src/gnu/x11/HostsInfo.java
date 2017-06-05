package gnu.x11;


public class HostsInfo {

    public Host.AccessControl mode;

    Host[] hosts;

    HostsInfo(ResponseInputStream in) {

        mode = Host.AccessControl.getControl(in.readBool());
        in.skip(6);
        int num_hosts = in.readInt16();
        in.skip(22);
        hosts = new Host[num_hosts];
        for (int i = 0; i < num_hosts; i++)
            hosts[i] = new Host(in);
    }
}
