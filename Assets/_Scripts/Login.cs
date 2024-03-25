using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Login : MonoBehaviour
{
    public Canvas show;
    private Canvas c;
    private void Awake()
    {
        c = GetComponent<Canvas>();
    }
    private void Update()
    {
        if (Input.GetMouseButtonDown(0))
        {
            c.hidden();
        }
    }
    private void OnDisable()
    {
        show.Show();
        AudioManager.Instance.OpenAudio();
    }
}
