
% Eventuality types and thematic roles

ct_Eventuality(soaPay).

ct_ThematicRole(soaHas_agent).
ct_ThematicRole(soaHas_object).
ct_ThematicRole(soaHas_instrument).


% Input facts
meta_triple(nopay, soa_enpj, type, permitted).
ct_triple(nopay, type, false).
ct_triple(nopay, type, hold).

ct_triple(soa_enpj, not, soa_epj).

ct_triple(soa_epj, type, soaPay).
ct_triple(soa_epj, soaHas_agent, soa_John).

ct_triple(soa_epj3, type, rexist).
ct_triple(soa_epj3, type, soaPay).
ct_triple(soa_epj3, soaHas_agent, soa_John).
ct_triple(soa_epj3, soaHas_object, amount(3, pounds)).
