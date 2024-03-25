using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;

public class Money : MonoBehaviour
{
    private Text text;
    private void Awake()
    {
        text = GetComponent<Text>();
    }
    private void OnEnable()
    {
        text.text = GameSystem.Instance.money.ToString();
    }
}
