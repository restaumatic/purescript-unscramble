function DecodingError(message) {
  this.message = message;
}

function decodingError(msg) {
  throw new DecodingError(msg);
}

exports.decodingError = decodingError;

// catchDecodingErrorImpl :: forall a r. (String -> r) -> (a -> r) -> (Partial => a) -> r
exports.catchDecodingErrorImpl = onError => onSuccess => fn => {
  try {
    return onSuccess(fn());
  } catch(e) {
    if(e instanceof DecodingError) {
      return onError(e.message);
    } else {
      throw e;
    }
  }
};

exports.unsafeParseJSON = json => {
  try {
    return JSON.parse(json);
  } catch(e) {
    if(e instanceof SyntaxError) {
      decodingError("Invalid JSON: " + e.message);
    } else {
      throw e;
    }
  }
};

const isString = x => typeof x === 'string';

exports.isString = isString;

exports.decodeString = x => {
  if(isString(x)) {
    return x;
  } else {
    decodingError("Expected String");
  }
};

const isNumber = x => typeof x === 'number';

exports.isNumber = isNumber;

exports.decodeNumber = x => {
  if(isNumber(x)) {
    return x;
  } else {
    decodingError("Expected Number");
  }
};

exports.decodeInt = x => {
  if(typeof x === 'number' && Math.floor(x) === x) {
    return x;
  } else {
    decodingError("Expected Int");
  }
};

exports.decodeBoolean = x => {
  if(typeof x === 'boolean') {
    return x;
  } else {
    decodingError("Expected Boolean");
  }
};

const expectArray = x => {
  if(typeof x === 'object' && x instanceof Array) {
    return x;
  } else {
    decodingError("Expected Array");
  }
};

exports.expectArray = expectArray;

exports.decodeArray = decodeItem => x => {
  return expectArray(x).map((item, index) => {
    try {
      return decodeItem(item);
    } catch(e) {
      if(e instanceof DecodingError) {
        decodingError('at [' + index + ']: ' + e.message);
      } else {
        throw e;
      }
    }
  });
};

const expectObject = x => {
  if(typeof x === 'object' && !(x instanceof Array)) {
    return x;
  } else {
    decodingError("Expected Object");
  }
};

exports.expectObject = expectObject;

exports.decodeObject = decodeItem => input => {
  const x = expectObject(input);
  const result = {};
  for(const key in x) {
    try {
      result[key] = decodeItem(x[key]);
    } catch(e) {
      if(e instanceof DecodingError) {
        decodingError('at .' + key + ': ' + e.message);
      } else {
        throw e;
      }
    }
  }
  return result;
};

/// Record decoding

exports.recordInfoCons = label => decodeItem => next => ({ label, decodeItem, next });
exports.recordInfoNil = null;

exports.decodeRecord = info => x => {
  if(typeof x === 'object' && !(x instanceof Array)) {
    const result = {};
    let entry = info;
    while(entry) {
      try {
        result[entry.label] = entry.decodeItem(x[entry.label]);
      } catch(e) {
        if(e instanceof DecodingError) {
          decodingError('at .' + entry.label + ': ' + e.message);
        } else {
          throw e;
        }
      }
      entry = entry.next;
    }
    return result;
  } else {
    decodingError("Expected Object");
  }
};
