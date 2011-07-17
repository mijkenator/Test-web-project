%% This module automatically generated - *do not edit*
%% Provides database indexes to get the direct offset in dynamic queries

-module(db_auto_index).
-export([get_index/2]).


%% account index
get_index(account, id) -> 2;
get_index(account, organization_id) -> 3;
get_index(account, name) -> 4;
get_index(account, password) -> 5;
get_index(account, email) -> 6;
get_index(account, active) -> 7;
get_index(account, jid) -> 8;
get_index(account, status) -> 9;
get_index(account, remind_code) -> 10;
get_index(account, need_new_password) -> 11;
get_index(account, created) -> 12;
get_index(account, login) -> 13;
get_index(account, client_version) -> 14;
get_index(account, last_login) -> 15;
get_index(account, avatar) -> 16;

%% organization index
get_index(organization, id) -> 2;
get_index(organization, name) -> 3;
get_index(organization, reseller_id) -> 4;
get_index(organization, active) -> 5;
get_index(organization, created) -> 6;

%% reseller index
get_index(reseller, id) -> 2;
get_index(reseller, name) -> 3;
get_index(reseller, password) -> 4;
get_index(reseller, email) -> 5;
get_index(reseller, active) -> 6;
get_index(reseller, info) -> 7;
get_index(reseller, created) -> 8;

%% orgadmin index
get_index(orgadmin, id) -> 2;
get_index(orgadmin, organization_id) -> 3;
get_index(orgadmin, reseller_id) -> 4;
get_index(orgadmin, name) -> 5;
get_index(orgadmin, password) -> 6;
get_index(orgadmin, email) -> 7;
get_index(orgadmin, active) -> 8;
get_index(orgadmin, info) -> 9;
get_index(orgadmin, created) -> 10;

%% device index
get_index(device, id) -> 2;
get_index(device, organization_id) -> 3;
get_index(device, account_id) -> 4;
get_index(device, name) -> 5;
get_index(device, password) -> 6;
get_index(device, type) -> 7;
get_index(device, mac) -> 8;
get_index(device, active) -> 9;
get_index(device, info) -> 10;
get_index(device, created) -> 11;

%% device_line index
get_index(device_line, id) -> 2;
get_index(device_line, device_id) -> 3;
get_index(device_line, name) -> 4;
get_index(device_line, auth_name) -> 5;
get_index(device_line, auth_passwd) -> 6;
get_index(device_line, proxy_addr) -> 7;
get_index(device_line, proxy_port) -> 8;

%% phone_number index
get_index(phone_number, id) -> 2;
get_index(phone_number, organization_id) -> 3;
get_index(phone_number, account_id) -> 4;
get_index(phone_number, number) -> 5;
get_index(phone_number, type) -> 6;
get_index(phone_number, active) -> 7;
get_index(phone_number, info) -> 8;
get_index(phone_number, created) -> 9;

%% acd index
get_index(acd, id) -> 2;
get_index(acd, acd_id) -> 3;
get_index(acd, account_id) -> 4;
get_index(acd, number) -> 5;
get_index(acd, active) -> 6;

%% acd_rules index
get_index(acd_rules, id) -> 2;
get_index(acd_rules, acd_id) -> 3;
get_index(acd_rules, name) -> 4;
get_index(acd_rules, time_period) -> 5;
get_index(acd_rules, priority) -> 6;
get_index(acd_rules, active) -> 7;

%% acd_rule_numbers index
get_index(acd_rule_numbers, id) -> 2;
get_index(acd_rule_numbers, acd_rule_id) -> 3;
get_index(acd_rule_numbers, order) -> 4;
get_index(acd_rule_numbers, forward_type) -> 5;
get_index(acd_rule_numbers, number) -> 6;
get_index(acd_rule_numbers, timeout) -> 7;
get_index(acd_rule_numbers, active) -> 8.