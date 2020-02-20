'use strict';

const AWS = require('../Network.AWS').aws();

exports._get = function(documentClient, params) {
    return documentClient.get(params).promise();
};

exports._delete = function (documentClient, params) {
    return documentClient.delete(params).promise();
};

exports._put = function (documentClient, params) {
    return documentClient.put(params).promise();
};

exports._scan = function (documentClient, params) {
    return documentClient.scan(params).promise();
};
