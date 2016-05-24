/// <reference path="../../lib/node.d.ts" />
/// <reference path="../../lib/request.d.ts" />

import config from '../config';
import request from 'request';

export function
findById(id: string, fn): void {
    var url = config.apiHost + '/objects/' + id;

    request(url, function(err, response, body) {
        if (err) {
            fn(err);
        } else if (response && response.statusCode == 200) {
            fn(null, JSON.parse(body).content);
        } else {
            fn(new Error('' + response.statusCode));
        }
    });
}
