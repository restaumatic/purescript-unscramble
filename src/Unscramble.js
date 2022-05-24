function DecodingError(message) {
  this.message = message;
}

function decodingError(msg) {
  throw new DecodingError(msg);
}

export {decodingError};

// catchDecodingErrorImpl :: forall a r. (String -> r) -> (a -> r) -> (Partial => a) -> r
export function catchDecodingErrorImpl(onError) {
  return onSuccess => fn => {
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
}

export function unsafeParseJSON(json) {
  try {
    return JSON.parse(json);
  } catch(e) {
    if(e instanceof SyntaxError) {
      decodingError("Invalid JSON: " + e.message);
    } else {
      throw e;
    }
  }
}

const isString = x => typeof x === 'string';

export {isString};

export function decodeString(x) {
  if(isString(x)) {
    return x;
  } else {
    decodingError("Expected String");
  }
}

const isNumber = x => typeof x === 'number';

export {isNumber};

export function decodeNumber(x) {
  if(isNumber(x)) {
    return x;
  } else {
    decodingError("Expected Number");
  }
}

export function decodeInt(x) {
  if(typeof x === 'number' && Math.floor(x) === x) {
    return x;
  } else {
    decodingError("Expected Int");
  }
}

export function decodeBoolean(x) {
  if(typeof x === 'boolean') {
    return x;
  } else {
    decodingError("Expected Boolean");
  }
}

const expectArray = x => {
  if(typeof x === 'object' && x instanceof Array) {
    return x;
  } else {
    decodingError("Expected Array");
  }
};

export {expectArray};

export function decodeArray(decodeItem) {
  return x => {
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
}

const expectObject = x => {
  if(typeof x === 'object' && !(x instanceof Array)) {
    return x;
  } else {
    decodingError("Expected Object");
  }
};

export {expectObject};

export function decodeObject(decodeItem) {
  return input => {
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
}

/// Record decoding

export function recordInfoCons(label) {
  return decodeItem => next => ({ label, decodeItem, next });
}

export var recordInfoNil = null;

export function decodeRecord(info) {
  return function decodeRecord$1(value) {
    const x = expectObject(value);

    const result = {};
    let entry = info;
    while(entry) {
      const label = entry.label;
      const original = x[label];
      let decoded;
      try {
        decoded = entry.decodeItem(original);
      } catch(e) {
        if(e instanceof DecodingError) {
          decodingError('at .' + entry.label + ': ' + e.message);
        } else {
          throw e;
        }
      }
      result[label] = decoded;
      entry = entry.next;
    }
    return result;
  };
}
