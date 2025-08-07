
---

# YooKassa/ЮКасса API Client for Free Pascal

Обёртка для [YooKassa API](https://yookassa.ru/developers) на языке Free Pascal (Lazarus / FPC).

Поддерживает:
- Создание и получение платежей
- Отправку чеков (ФФД 1.05 и выше)
- Агентские схемы
- Вебхуки (webhook)
- Логирование и валидация

---

## 🚀 Быстрый старт

### 1. Установка

Скопируйте файлы в ваш проект:
  Просто добавьте пакет `yookassa_rt.lpk` к вашему проекту 

Добавьте по необходимости нужные юниты в `uses`:
```pascal
uses yookassa_api, yookassa_exceptions, yookassa_webhook;
```

---

### 2. Создание платежа

```pascal
var
  aPayment: TYookassaCreatePaymentRequest;
  aResp: TYookassaPaymentResponse;
begin
  aPayment := TYookassaCreatePaymentRequest.Create;
  try
    aPayment.ShopId := 'your_shop_id';
    aPayment.SecretKey := 'your_secret_key';
    aPayment.Amount := 999.99;
    aPayment.Currency := 'RUB';
    aPayment.Description := 'Оплата заказа №123';
    aPayment.ReturnUrl := 'https://example.com/return';

    aResp := aPayment.Execute as TYookassaPaymentResponse;
    try
      WriteLn('Ссылка для оплаты: ', aResp.ConfirmationURL);
      WriteLn('ID платежа: ', aResp.GetId);
    finally
      aResp.Free;
    end;
  finally
    aPayment.Free;
  end;
end;
```

---

### 3. Отправка чека

```pascal
var
  aReceiptReq: TYookassaCreateReceiptRequest;
  aItem: TYookassaReceiptItem;
  aResp: TYookassaReceiptResponse;
begin
  aReceiptReq := TYookassaCreateReceiptRequest.Create;
  try
    aReceiptReq.ShopId := 'your_shop_id';
    aReceiptReq.SecretKey := 'your_secret_key';
    aReceiptReq.ReceiptType := 'payment';
    aReceiptReq.PaymentId := 'pay_123abc';
    aReceiptReq.Send := True;

    // Добавляем товар
    aItem := TYookassaReceiptItem.Create;
    aItem.Description := 'Книга "Pascal для начинающих"';
    aItem.Quantity := 1.0;
    aItem.AmountValue := 999.99;
    aItem.AmountCurrency := 'RUB';
    aItem.VatCode := 1; // НДС 18%
    aItem.PaymentMode := 'full_prepayment';
    aItem.PaymentSubject := 'commodity';
    aReceiptReq.Items.Add(aItem);

    // Клиент
    aReceiptReq.CustomerEmail := 'client@example.com';

    aResp := aReceiptReq.Execute as TYookassaReceiptResponse;
    WriteLn('Чек отправлен: ', aResp.GetId);
  finally
    aReceiptReq.Free;
  end;
end;
```

---

### 4. Получение информации о платеже

```pascal
var
  aGetPayment: TYookassaGetPaymentRequest;
  aResp: TYookassaPaymentResponse;
begin
  aGetPayment := TYookassaGetPaymentRequest.Create;
  try
    aGetPayment.ShopId := 'your_shop_id';
    aGetPayment.SecretKey := 'your_secret_key';
    aGetPayment.PaymentId := 'pay_123abc';

    aResp := aGetPayment.Execute as TYookassaPaymentResponse;
    WriteLn('Статус: ', aResp.GetStatus);
    WriteLn('Сумма: ', aResp.Amount);
  finally
    aGetPayment.Free;
    aResp.Free;
  end;
end;
```

---

## 🛠 Расширенные возможности

### Агентские схемы (поставщик)

```pascal
aItem.Supplier.Name := 'ИП Петров';
aItem.Supplier.Phone := '+79001234567';
aItem.Supplier.Inn := '123456789012';
```

### Тип посредника (ФФД 1.1)

```pascal
aItem.AgentType := atPaymentAgent; // Платёжный агент
```

### Маркировка (GS1M)

```pascal
aItem.MarkMode := 2; // Товар подлежит маркировке
aItem.MarkCodeInfo := 'VGVzdE1hcmtDb2RlMTIzNDU2Nzg5MA=='; // base64
```

### Перечисление на счёт, телефон или кошелёк

```pascal
aPayment.Receiver.ReceiverType := rtBankAccount;
aPayment.Receiver.AccountNumber := '12345678901234567890';
aPayment.Receiver.Bic := '044525225';
```

---

## 🔔 Вебхуки (Webhook)

### Пример для FCL-web

```pascal
procedure HandleWebhook(ARequest: TFPHTTPConnectionRequest; AResponse: TFPHTTPConnectionResponse);
var
  aBody: string;
  aHandler: TYookassaWebhookHandler;
begin
  aBody := ARequest.Content;

  aHandler := TYookassaWebhookHandler.Create('unused');
  try
    aHandler.OnPaymentSucceeded := @OnPaymentSucceeded;
    aHandler.OnPaymentWaitingForCapture := @OnPaymentWaitingForCapture;
    aHandler.OnLog := @OnLog; // опционально

    AResponse.Content := aHandler.HandleWebhook(aBody);
    AResponse.ContentType := 'application/json';
    AResponse.StatusCode := 200;
  finally
    aHandler.Free;
  end;
end;
```

### Обработчик события

```pascal
procedure OnPaymentSucceeded(const aEvent: TYookassaWebhookData);
begin
  WriteLn('Платёж ', aEvent.ObjectId, ' успешно оплачен!');
  // Обновить статус заказа, отправить email и т.д.
end;
```

---

## 🧪 Тестирование

Модуль включает:
- **Юнит-тесты** (`test_yookassa_api.pas`)
- **Интеграционные тесты** (`test_yookassa_api_integration.pas`)

Запустите тесты, чтобы проверить:
- Сериализацию JSON
- Валидацию входных данных
- Работу с API

---

## 📦 Поддерживаемые фреймворки

`TYookassaWebhookHandler` абстрагирован от веб-сервера. Работает с:
- `fcl-web` (`TFPHTTPServer`)
- `BrookFramework`
- `BrookFreePascal` (`TBrookAction`)
- CGI, FastCGI

---

## 🛡 Безопасность

- **Валидация входных данных** — `EYooKassaValidationError`
- **Логирование** — все запросы и ответы
- **Idempotence-Key** — на основе GUID
- **Секретный URL вебхука** — для защиты от подделки

---

## 📚 Документация

- [API YooKassa](https://yookassa.ru/developers/api)
- [ФФД 1.1](https://yookassa.ru/developers/payment-acceptance/receipts/54fz/parameters-values)

---

## 📄 Лицензия

MIT

---
