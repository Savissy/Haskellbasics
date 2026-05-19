That’s actually very common with newer Outlook security settings and company email policies. Microsoft often blocks VBA from silently sending emails for security reasons.

The good news is:
your VBA system is probably working correctly — Outlook is just refusing automatic sending.

There are 3 reliable solutions.

---

# BEST SOLUTION (RECOMMENDED)

## Use `.Display` Instead of `.Send`

This is the safest and most reliable option.

Replace:

```vb id="41m7dz"
.Send
```

with:

```vb id="zjlwmv"
.Display
```

What happens:

* Outlook opens the email automatically,
* staff only clicks:

```text id="4ofm6d"
Send
```

This bypasses most Outlook security restrictions.

For many companies, this is enough.

---

# BETTER AUTOMATION SOLUTION (MOST PROFESSIONAL)

## Use Power Automate + Excel

This is MUCH better than VBA email sending.

Why?
Because:

* works even if Excel is closed,
* no Outlook security blocking,
* cloud-based,
* automatic reminders,
* works on company systems reliably.

This is what many businesses now use instead of VBA emailing.

---

# WHAT POWER AUTOMATE CAN DO

Automatically:

* read Excel sheet,
* detect expiring products,
* send email alerts,
* send Teams notifications,
* schedule daily checks,
* work in background,
* work even when nobody opens Excel.

---

# HOW TO SET IT UP (STEP-BY-STEP)

---

# STEP 1 — STORE EXCEL FILE IN ONEDRIVE

VERY IMPORTANT.

Move your Excel file into:

* OneDrive
  OR
* SharePoint

Example:

```text id="gq4r4n"
OneDrive/Documents/Inventory_Expiry_System.xlsm
```

Power Automate works best with cloud-stored Excel files.

---

# STEP 2 — CREATE TABLE INSIDE EXCEL

Power Automate needs a proper Excel Table.

---

## HOW

Select your inventory data.

Press:

```text id="2zn56k"
CTRL + T
```

Tick:

```text id="e89jlx"
My table has headers
```

Click OK.

---

# STEP 3 — NAME THE TABLE

Click inside table.

At top-left:

```text id="l5p6g1"
Table Design
```

Rename table:

```text id="k3iwk7"
InventoryTable
```

VERY IMPORTANT.

---

# STEP 4 — OPEN POWER AUTOMATE

Go to:

[Microsoft Power Automate](https://make.powerautomate.com?utm_source=chatgpt.com)

Sign in using company Microsoft account.

---

# STEP 5 — CREATE AUTOMATED FLOW

Click:

```text id="dtjlwm"
Create
```

Then:

```text id="rgkmx8"
Scheduled Cloud Flow
```

---

# STEP 6 — CONFIGURE SCHEDULE

Example:

* Daily
* 8:00 AM

Click:

```text id="jlwm7j"
Create
```

---

# STEP 7 — ADD EXCEL ACTION

Search:

```text id="wdyw6n"
Excel Online (Business)
```

Choose:

```text id="2vghpx"
List rows present in a table
```

---

# STEP 8 — SELECT FILE

Choose:

* OneDrive location
* Excel file
* Table:

```text id="c6rjmf"
InventoryTable
```

---

# STEP 9 — ADD CONDITION

Add condition:

Check:

```text id="jlwm4l"
Days Left <= 30
```

This filters expiring products.

---

# STEP 10 — SEND EMAIL

Add action:

```text id="jlwm8j"
Send an email (V2)
```

Use:

* company email,
* manager email,
* admin email.

---

# EXAMPLE EMAIL BODY

```text id="jlwmxw"
The following products are nearing expiry:

Milk A - 5 days left
Juice B - 12 days left
```

---

# FINAL RESULT

Every day:

* Power Automate checks Excel automatically,
* detects expiring products,
* sends emails,
* no Excel opening needed,
* no VBA blocking,
* no Outlook security issues.

---

# THIRD OPTION (ADVANCED VBA FIX)

If you MUST use VBA silent sending:

You can use:

* Outlook Redemption Library,
* CDO Mail,
* SMTP integration.

But:

* setup is technical,
* sometimes blocked by company IT policies,
* less reliable than Power Automate.

So I strongly recommend:

* Power Automate,
  OR
* `.Display`.

---

# WHAT I RECOMMEND FOR YOUR COMPANY

## BEST LONG-TERM SETUP

### Excel + OneDrive + Power Automate

This gives:

✅ Automatic monitoring
✅ Automatic emails
✅ Background checking
✅ No Outlook blocking
✅ Multiple staff support
✅ Cloud automation
✅ Professional workflow
✅ No need to open Excel

This is the modern business-standard solution.

Sn
Date
Name of material 
Suppliers name
Date of analysis 
Source (local/imported)
Production date
Expiry date
Batch no
Container no/truck no
Waybill no
Coa yes/no
Unit/pack
Quantity
Qty waybilled
Qty supplied
Qty passed
QA visual inspections (tamper proof, damages,labels,others)
Qty rejected
Shortages
Comments


