unit yookassa_constants;

{$mode objfpc}{$H+}

interface

const
  _YOOKASSA_DEFAULT_API_URL = 'https://api.yookassa.ru/v3';

  // HTTP Methods
  _HTTP_METHOD_GET = 'GET';
  _HTTP_METHOD_POST = 'POST';

  // HTTP Headers
  _HEADER_AUTHORIZATION = 'Authorization';
  _HEADER_CONTENT_TYPE = 'Content-Type';
  _HEADER_IDEMPOTENCE_KEY = 'Idempotence-Key';

  // Content Types
  _CONTENT_TYPE_JSON = 'application/json';

  // API Endpoints
  _ENDPOINT_PAYMENTS = '/payments';
  _ENDPOINT_RECEIPTS = '/receipts'; { #todo
  ENDPOINT_REFUNDS = '/refunds';
  ENDPOINT_PAYOUTS = '/payouts';
  ENDPOINT_DEALS = '/deals';
  ENDPOINT_PAYMENT_METHODS = '/payment_methods';    }

  // Payment Statuses
  _PAYMENT_STATUS_PENDING = 'pending';
  _PAYMENT_STATUS_WAITING_FOR_CAPTURE = 'waiting_for_capture';
  _PAYMENT_STATUS_SUCCEEDED = 'succeeded';
  _PAYMENT_STATUS_CANCELED = 'canceled';

  // Refund statuses
  _REFUND_STATUS_SUCCEEDED = 'succeeded';

  // Deal status
  _DEAL_STATUS_CLOSED = 'closed';

  // Payout statuses
  _PAYOUT_STATUS_SUCCEEDED = 'succeeded';
  _PAYOUT_STATUS_CANCELED = 'canceled';

  // Payment method status
  _PAYMENTMETHOD_STATUS_ACTIVE = 'active';

  // Receipt Types
  _RECEIPT_TYPE_PAYMENT = 'payment';
  _RECEIPT_TYPE_REFUND = 'refund';

  // Payment Modes (ФФД)
  _PAYMENT_MODE_FULL_PREPAYMENT = 'full_prepayment';
  _PAYMENT_MODE_FULL_PAYMENT = 'full_payment';

  // Payment Subjects (ФФД)
  { #todo
  PAYMENT_SUBJECT_COMMODITY = 'commodity';
  PAYMENT_SUBJECT_EXCISE = 'excise';
  PAYMENT_SUBJECT_JOB = 'job';
  PAYMENT_SUBJECT_SERVICE = 'service';
  PAYMENT_SUBJECT_GAMBLING_BET = 'gambling_bet';
  PAYMENT_SUBJECT_GAMBLING_PRIZE = 'gambling_prize';
  PAYMENT_SUBJECT_LOTTERY = 'lottery';
  PAYMENT_SUBJECT_LOTTERY_PRIZE = 'lottery_prize';
  PAYMENT_SUBJECT_INTELLECTUAL_ACTIVITY = 'intellectual_activity';
  PAYMENT_SUBJECT_PAYMENT = 'payment';
  PAYMENT_SUBJECT_AGENT_COMMISSION = 'agent_commission';
  PAYMENT_SUBJECT_PROPERTY_RIGHT = 'property_right';
  PAYMENT_SUBJECT_NON_OPERATING_GAIN = 'non_operating_gain';
  PAYMENT_SUBJECT_INSURANCE_PREMIUM = 'insurance_premium';
  PAYMENT_SUBJECT_SALES_TAX = 'sales_tax';
  PAYMENT_SUBJECT_RESORT_FEE = 'resort_fee';
  PAYMENT_SUBJECT_COMPOSITE = 'composite';
  PAYMENT_SUBJECT_ANOTHER = 'another';           }

  // Agent Types (ФФД 1.1)
  _AGENT_TYPE_BANKING_PAYMENT_AGENT = 'banking_payment_agent';
  _AGENT_TYPE_BANKING_PAYMENT_SUBAGENT = 'banking_payment_subagent';
  _AGENT_TYPE_PAYMENT_AGENT = 'payment_agent';
  _AGENT_TYPE_PAYMENT_SUBAGENT = 'payment_subagent';
  _AGENT_TYPE_ATTORNEY = 'attorney';
  _AGENT_TYPE_COMMISSIONER = 'commissioner';
  _AGENT_TYPE_AGENT = 'agent';

  // Receiver Types
  _RECEIVER_TYPE_BANK_ACCOUNT = 'bank_account';
  _RECEIVER_TYPE_MOBILE_BALANCE = 'mobile_balance';
  _RECEIVER_TYPE_DIGITAL_WALLET = 'digital_wallet';

  // Settlement Types
  _SETTLEMENT_TYPE_CASHLESS = 'cashless';
  _SETTLEMENT_TYPE_PREPAYMENT = 'prepayment';
  _SETTLEMENT_TYPE_POSTPAYMENT = 'postpayment';
  _SETTLEMENT_TYPE_CONSIDERATION = 'consideration';

  // Confirmation Types
  _CONFIRMATION_TYPE_REDIRECT = 'redirect';

  // Webhook Object Types
  _WEBHOOK_OBJECT_TYPE_PAYMENT = 'payment';
  _WEBHOOK_OBJECT_TYPE_REFUND = 'refund';
  _WEBHOOK_OBJECT_TYPE_PAYOUT = 'payout';
  _WEBHOOK_OBJECT_TYPE_DEAL = 'deal';
  _WEBHOOK_OBJECT_TYPE_PAYMENT_METHOD = 'paymentmethod';

  // Webhook Events
  WEBHOOK_EVENT_PAYMENT_SUCCEEDED =           _WEBHOOK_OBJECT_TYPE_PAYMENT+'.'+_PAYMENT_STATUS_SUCCEEDED;
  WEBHOOK_EVENT_PAYMENT_WAITING_FOR_CAPTURE = _WEBHOOK_OBJECT_TYPE_PAYMENT+'.'+_PAYMENT_STATUS_WAITING_FOR_CAPTURE;
  WEBHOOK_EVENT_PAYMENT_CANCELED =            _WEBHOOK_OBJECT_TYPE_PAYMENT+'.'+_PAYMENT_STATUS_CANCELED;
  WEBHOOK_EVENT_REFUND_SUCCEEDED =            _WEBHOOK_OBJECT_TYPE_REFUND+'.'+ _REFUND_STATUS_SUCCEEDED;
  WEBHOOK_EVENT_PAYOUT_SUCCEEDED =            _WEBHOOK_OBJECT_TYPE_PAYOUT+'.'+ _PAYOUT_STATUS_SUCCEEDED;
  WEBHOOK_EVENT_PAYOUT_CANCELED =             _WEBHOOK_OBJECT_TYPE_REFUND+'.'+ _PAYOUT_STATUS_CANCELED;
  WEBHOOK_EVENT_DEAL_CLOSED =                 _WEBHOOK_OBJECT_TYPE_DEAL+  '.'+ _DEAL_STATUS_CLOSED;
  WEBHOOK_EVENT_PAYMENT_METHOD_ACTIVE =       _WEBHOOK_OBJECT_TYPE_PAYMENT_METHOD+'.'+_PAYMENTMETHOD_STATUS_ACTIVE;

  // Currency Codes
  CURRENCY_RUB = 'RUB';
  CURRENCY_USD = 'USD';
  CURRENCY_EUR = 'EUR';
  CURRENCY_KZT = 'KZT';
  CURRENCY_BYN = 'BYN';

  // Default Values
  _DEFAULT_CURRENCY = CURRENCY_RUB;
  _DEFAULT_CAPTURE = True;
  _DEFAULT_SEND_RECEIPT = True;
  _DEFAULT_TAX_SYSTEM_CODE = -1; // Not specified
  _DEFAULT_MARK_MODE = -1; // Not specified

  // Validation Limits
  _MIN_MARK_CODE_LENGTH = 30;
  _MAX_MARK_CODE_LENGTH = 100;

  // JSON Field Names
  _JSON_FIELD_ID = 'id';
  _JSON_FIELD_TYPE = 'type';
  _JSON_FIELD_STATUS = 'status';
  _JSON_FIELD_EVENT = 'event';
  _JSON_FIELD_OBJECT = 'object';
  _JSON_FIELD_AMOUNT = 'amount';
  _JSON_FIELD_VALUE = 'value';
  _JSON_FIELD_CURRENCY = 'currency';
  _JSON_FIELD_DESCRIPTION = 'description';
  _JSON_FIELD_CONFIRMATION = 'confirmation';
  _JSON_FIELD_CONFIRMATION_URL = 'confirmation_url';
  _JSON_FIELD_RETURN_URL = 'return_url';
  _JSON_FIELD_CAPTURE = 'capture';
  _JSON_FIELD_RECEIPT = 'receipt';
  _JSON_FIELD_CUSTOMER = 'customer';
  _JSON_FIELD_EMAIL = 'email';
  _JSON_FIELD_PHONE = 'phone';
  _JSON_FIELD_FULL_NAME = 'full_name';
  _JSON_FIELD_INN = 'inn';
  _JSON_FIELD_ITEMS = 'items';
  _JSON_FIELD_QUANTITY = 'quantity';
  _JSON_FIELD_VAT_CODE = 'vat_code';
  _JSON_FIELD_PAYMENT_MODE = 'payment_mode';
  _JSON_FIELD_PAYMENT_SUBJECT = 'payment_subject';
  _JSON_FIELD_PAYMENT_ID = 'payment_id';
  _JSON_FIELD_REFUND_ID = 'refund_id';
  _JSON_FIELD_SEND = 'send';
  _JSON_FIELD_SETTLEMENTS = 'settlements';
  _JSON_FIELD_TAX_SYSTEM_CODE = 'tax_system_code';
  _JSON_FIELD_MARK_MODE = 'mark_mode';
  _JSON_FIELD_MARK_CODE_INFO = 'mark_code_info';
  _JSON_FIELD_GS_1M = 'gs_1m';
  _JSON_FIELD_MEASURE = 'measure';
  _JSON_FIELD_AGENT_TYPE = 'agent_type';
  _JSON_FIELD_SUPPLIER = 'supplier';
  _JSON_FIELD_NAME = 'name';
  _JSON_FIELD_RECEIVER = 'receiver';
  _JSON_FIELD_ACCOUNT_NUMBER = 'account_number';
  _JSON_FIELD_BIC = 'bic';
  _JSON_FIELD_METADATA = 'metadata';
  _JSON_FIELD_ORDER_ID = 'order_id';

  // Error Messages
  _ERR_INVALID_BASE64 = '%s is not a valid base64 string';
  _ERR_INVALID_MARK_CODE_LENGTH = '%s has invalid length after decoding (expected %d–%d bytes)';
  _ERR_MARK_CODE_REQUIRED = 'MarkCodeInfo is required when MarkMode is set';
  _ERR_MARK_CODE_DECODE = 'MarkCodeInfo decoding error: %s';
  _ERR_RECEIPT_ITEMS_REQUIRED = 'Receipt must have at least one item';
  _ERR_REFUND_ID_REQUIRED = 'For refund receipt, either PaymentId or RefundId must be specified';
  _ERR_HTTP_METHOD_NOT_SUPPORTED = 'HTTP method not supported: %s';

  // Response Messages
  _RESPONSE_STATUS_OK = '{"status": "ok"}';
  _RESPONSE_ERROR_BAD_REQUEST = '{"error": "Bad Request"}';
  _RESPONSE_ERROR_INVALID_JSON = '{"error": "Invalid JSON"}';
  _RESPONSE_ERROR_INVALID_JSON_STRUCTURE = '{"error": "Invalid JSON structure"}';
  _RESPONSE_ERROR_DATA_PROCESSING = '{"error": "Data processing error"}';

  // Log Messages
  _LOG_REQUEST_FORMAT = 'Request (Idempotence-Key - %s) (%s %s):%s%s';
  _LOG_RESPONSE_FORMAT = 'Response (status: %d): %s';
  _LOG_WEBHOOK_RECEIVED = 'Webhook: Received raw body: %s';
  _LOG_WEBHOOK_JSON_PARSED = 'Webhook: JSON parsed successfully';
  _LOG_WEBHOOK_INVALID_JSON = 'Webhook: Invalid JSON - %s';
  _LOG_WEBHOOK_DATA_FAILED = 'Webhook: Data creation failed - %s';
  _LOG_WEBHOOK_PROCESSING = 'Webhook: Processing event "%s"';
  _LOG_WEBHOOK_TRIGGERING = 'Webhook: Triggering handler for %s';
  _LOG_WEBHOOK_NO_HANDLER = 'Webhook: No handler found for event: %s';
  _LOG_WEBHOOK_ROOT_NOT_OBJECT = 'Webhook: Root element is not JSON object';

  // Basic Auth prefix
  _AUTH_BASIC_PREFIX = 'Basic ';

implementation

end.
