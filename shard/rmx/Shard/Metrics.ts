import * as url from 'url';
import influx from 'influx';


var client = null;


var influxUrl = process.env.INFLUX;
if (influxUrl) {
    var u = url.parse(influxUrl);
    var auth = u.auth.split(':');

    client = influx(
        { hosts    : [ { host: u.hostname, port: parseInt(u.port, 10) } ]
        , database : u.pathname.slice(1, u.pathname.length)
        , username : auth[0]
        , password : auth[1]
        }
    );
}


export function
writePoint(series: string, point, tags): void {
    if (client) {
        client.writePoint(series, point, tags, () => {});
    }
}
