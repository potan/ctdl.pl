
% Eventuality types and thematic roles

ct_Evetuality(soaLeave).
ct_Evetuality(soaPay).

ct_ThematicRole(soaHas_agent).
ct_ThematicRole(soaHas_object).
ct_ThematicRole(soaHas_instrument).


% Input facts
ct_triple(soa_elj, type, rexist).
ct_triple(soa_elj, type, soaLeave).
ct_triple(soa_elj, soaHas_agent, soa_John).

ct_triple(soa_enlj, not, soa_elj).
ct_triple(soa_enlj, type, rexist).

ct_triple(soa_epj, type, rexist).
ct_triple(soa_epj, type, soaPay).
ct_triple(soa_epj, soaHas_agent, soa_John).
ct_triple(soa_epj, soaHas_object, amount(3, pounds)).
ct_triple(soa_epj, soaHas_instrument, soa_cash).

ct_triple(soa_enpj, not, soa_epj).
ct_triple(soa_enpj, type, rexist).
