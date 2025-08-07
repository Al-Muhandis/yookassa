# Tests

This directory contains all unit and integration tests for the project.

## Unit Tests

Unit tests are designed to verify the functionality of individual components in isolation. You can find unit tests in this directory with the naming convention `test_yookassa_api*`. These tests do not require external services or a specific environment configuration.

## Integration Test

The integration test (`test_yookassa_api_integration*`) checks the interaction between different parts of the system and YooKassa API endpoint. To run the integration test, you need to provide valid configuration data in a `config.ini` file.

## Creating and Filling config.ini

Before running the integration test, create a `config.ini` file in the `tests` directory. This file must include your YooKassa API credentials and any other required configuration values.

Example of `config.ini`:
```ini
[shop]
ShopId = YOUR_SHOP_ID
secret_key = YOUR_SECRET_KEY

[order]
Amount=10.00
Currency=RUB
Description=FreePascal test for Yookassa API
ReturnUrl=https://example.com/return

[receipt]
customer.email=test-customer@example.com
# Add other configuration values if required
```

- Replace `YOUR_SHOP_ID` with your actual (you can use test shop) Shop ID from YooKassa.
- Replace `YOUR_SECRET_KEY` with your actual secret key from YooKassa.

---

If you have questions about test configuration or usage, please refer to the project's main documentation or open an issue.